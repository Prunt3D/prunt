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

pragma Style_Checks ("-s");
--  Lemmas are declared by their bodies, where they are proved: a separate declaration would only repeat them.
pragma Warnings ("F");
--  A lemma can take a parameter only for its contract, which is not compiled.

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

package body Prunt.Motion_Planner.Trajectory.Profile_Model.Optimality
  with SPARK_Mode => On
is

   ------------------------------------------------------------------------------------------------------------------
   --  How the proof goes
   --
   --  Take the spans A, B, C, D of a ramp, and As, Bs, Cs, Ds of the ramp it is compared with, which the comments
   --  call the star. The star is such that Bs = As unless A <= As, Cs = As + Bs unless A * B <= As * Bs, and
   --  Ds = As + Bs + Cs unless A * B * C <= As * Bs * Cs: where the star spends time in a later phase, the other
   --  ramp has no higher a peak below it. The claim is that A * B * C * D >= As * Bs * Cs * Ds implies
   --  A + B + C + D >= As + Bs + Cs + Ds.
   --
   --  Write P, M for the products of the first two and three spans and Q, S for their sums. The spans are taken
   --  away one at a time from the last:
   --
   --   - D is at least S and at least As * Bs * Cs * Ds / M, so the claim follows from S * Ms >= Ss * M, which is
   --     needed where M <= Ms or where S <= Ss (Lemma_Quadruple);
   --   - (Q + C) / (P * C) falls as C grows, and C is at most Ms / P or at most Ss - Q, so that follows from
   --     Q * Ps >= Qs * P, which is needed where P <= Ps or where Q <= Qs (Lemma_Triple);
   --   - (A + B) / (A * B) falls as B grows, and B is at most Ps / A or at most Qs - A, so that follows from
   --     A <= As, or from As = Bs (Lemma_Pair).
   --
   --  Every step is a polynomial identity whose right side is a sum of products of quantities which are not
   --  negative. The lemmas take scalar parameters, because the provers settle such identities at once on scalars
   --  and not on the components of a record.
   ------------------------------------------------------------------------------------------------------------------

   ------------------------------------------------------------------------------------------------------------------
   --  Arithmetic
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Nonnegative (X, Y : Real) with Pre => X >= 0.0 and then Y >= 0.0, Post => X * Y >= 0.0 is
   begin
      null;
   end Lemma_Nonnegative;

   procedure Lemma_Positive (X, Y : Real) with Pre => X > 0.0 and then Y > 0.0, Post => X * Y > 0.0 is
   begin
      null;
   end Lemma_Positive;

   procedure Lemma_Monotone (X1, X2, Y1, Y2 : Real)
   with Pre => X1 >= 0.0 and then X1 <= X2 and then Y1 >= 0.0 and then Y1 <= Y2, Post => X1 * Y1 <= X2 * Y2
   is
   begin
      pragma Assert (X1 * Y1 <= X2 * Y1);
      pragma Assert (X2 * Y1 <= X2 * Y2);
   end Lemma_Monotone;

   procedure Lemma_Cancel (K, X, Y : Real) with Pre => K > 0.0 and then K * X >= K * Y, Post => X >= Y is
   begin
      if X < Y then
         Lemma_Positive (K, Y - X);
         pragma Assert (K * (Y - X) = K * Y - K * X);
      end if;
   end Lemma_Cancel;

   procedure Lemma_Squares (X, Y : Real) with Post => X * X + Y * Y >= 2.0 * X * Y is
   begin
      if X >= Y then
         Lemma_Nonnegative (X - Y, X - Y);
      else
         Lemma_Nonnegative (Y - X, Y - X);
         pragma Assert ((Y - X) * (Y - X) = (X - Y) * (X - Y));
      end if;
      pragma Assert (X * X + Y * Y - 2.0 * X * Y = (X - Y) * (X - Y));
   end Lemma_Squares;

   procedure Lemma_Fraction (X, K : Real)
   with
     Pre  => K > 0.0 and then X >= 0.0 and then X <= K,
     Post => X / K >= 0.0 and then X / K <= 1.0 and then K * (X / K) = X
   is
   begin
      pragma Assert (K * (X / K) = X);
      if X / K < 0.0 then
         Lemma_Positive (K, 0.0 - X / K);
         pragma Assert (K * (0.0 - X / K) = 0.0 - K * (X / K));
      end if;
      if X / K > 1.0 then
         Lemma_Positive (K, X / K - 1.0);
         pragma Assert (K * (X / K - 1.0) = K * (X / K) - K);
      end if;
   end Lemma_Fraction;

   ------------------------------------------------------------------------------------------------------------------
   --  Two spans: (A + B) / (A * B) is least for the star
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Pair_Product_Bounded (A, B, As, Bs : Real)
   with
     Pre  =>
       A > 0.0
       and then B > 0.0
       and then As > 0.0
       and then Bs > 0.0
       and then A * B <= As * Bs
       and then (As = Bs or else (A <= As and then As <= Bs)),
     Post => (A + B) * (As * Bs) >= (As + Bs) * (A * B)
   is
      L : constant Real := As * Bs;
      E : constant Real := A * A + L - (As + Bs) * A;
   begin
      --  B is at most L / A.
      Lemma_Nonnegative (A, L - A * B);
      pragma Assert ((A + B) * L - (A * A + L) * B = A * (L - A * B));
      --  A + L / A is at least As + Bs, because A is not between As and Bs.
      if As = Bs then
         Lemma_Squares (A, As);
         pragma Assert (E = A * A + As * As - 2.0 * A * As);
      else
         Lemma_Nonnegative (As - A, Bs - A);
         pragma Assert (E = (As - A) * (Bs - A));
      end if;
      pragma Assert (E >= 0.0);
      Lemma_Nonnegative (E, B);
      pragma Assert (E * B = (A * A + L) * B - (As + Bs) * (A * B));
   end Lemma_Pair_Product_Bounded;

   procedure Lemma_Pair_Sum_Bounded (A, B, As, Bs : Real)
   with
     Pre  => A > 0.0 and then B > 0.0 and then A <= As and then As <= Bs and then A + B <= As + Bs,
     Post => (A + B) * (As * Bs) >= (As + Bs) * (A * B)
   is
      Q  : constant Real := A + B;
      Qs : constant Real := As + Bs;
   begin
      --  B is at most Qs - A, taking the sum with it.
      Lemma_Nonnegative (A, A);
      Lemma_Nonnegative (A * A, Qs - Q);
      pragma Assert (A * (Qs - A) * Q - (A * B) * Qs = (A * A) * (Qs - Q));
      --  A * (Qs - A) rises with A up to As.
      Lemma_Nonnegative (As - A, Bs - A);
      pragma Assert (As * Bs - A * (Qs - A) = (As - A) * (Bs - A));
      Lemma_Nonnegative (As * Bs - A * (Qs - A), Q);
      pragma Assert ((As * Bs) * Q - A * (Qs - A) * Q = (As * Bs - A * (Qs - A)) * Q);
   end Lemma_Pair_Sum_Bounded;

   procedure Lemma_Pair (A, B, As, Bs : Real)
   with
     Pre  =>
       A > 0.0
       and then B >= A
       and then As > 0.0
       and then Bs >= As
       and then (Bs = As or else A <= As)
       and then (A * B <= As * Bs or else A + B <= As + Bs),
     Post => (A + B) * (As * Bs) >= (As + Bs) * (A * B)
   is
   begin
      if A * B <= As * Bs then
         Lemma_Pair_Product_Bounded (A, B, As, Bs);
      else
         --  Where Bs = As, twice A is at most A + B, which is at most twice As.
         pragma Assert (A <= As);
         Lemma_Pair_Sum_Bounded (A, B, As, Bs);
      end if;
   end Lemma_Pair;

   ------------------------------------------------------------------------------------------------------------------
   --  Three spans: (Q + C) / (P * C) is least for the star, where P and Q are the product and the sum of the first
   --  two
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Triple_Longest (P, Q, C, L : Real)
   with Pre => Q >= 0.0 and then P * C <= L, Post => (Q + C) * L >= (Q * P + L) * C
   is
   begin
      Lemma_Nonnegative (Q, L - P * C);
      pragma Assert ((Q + C) * L - (Q * P + L) * C = Q * (L - P * C));
   end Lemma_Triple_Longest;
   --  C is at most L / P.

   procedure Lemma_Triple_Product_Bounded (P, Q, C, Ps, Qs, Cs : Real)
   with
     Pre  =>
       P > 0.0
       and then Q > 0.0
       and then C > 0.0
       and then Ps > 0.0
       and then Qs > 0.0
       and then Cs > 0.0
       and then P <= Ps
       and then P * C <= Ps * Cs
       and then Qs <= Cs
       and then Q * Ps >= Qs * P,
     Post => (Q + C) * (Ps * Cs) >= (Qs + Cs) * (P * C)
   is
      L : constant Real := Ps * Cs;
      E : constant Real := Q * P + L - (Qs + Cs) * P;
   begin
      Lemma_Triple_Longest (P, Q, C, L);
      Lemma_Nonnegative (P, Q * Ps - Qs * P);
      Lemma_Monotone (P, Ps, Qs, Cs);
      Lemma_Nonnegative (Ps - P, Ps * Cs - P * Qs);
      pragma Assert (Ps * E = P * (Q * Ps - Qs * P) + (Ps - P) * (Ps * Cs - P * Qs));
      Lemma_Cancel (Ps, E, 0.0);
      Lemma_Nonnegative (E, C);
      pragma Assert (E * C = (Q * P + L) * C - (Qs + Cs) * (P * C));
   end Lemma_Triple_Product_Bounded;
   --  The star's third span is longer than the sum of its first two.

   procedure Lemma_Triple_Product_Bounded_Short (P, Q, C, Ps, Qs : Real)
   with
     Pre  =>
       P > 0.0
       and then Q > 0.0
       and then C > 0.0
       and then Ps > 0.0
       and then Qs > 0.0
       and then P * C <= Ps * Qs
       and then Q <= C
       and then (Q >= Qs or else Q * Ps >= Qs * P),
     Post => (Q + C) * (Ps * Qs) >= (Qs + Qs) * (P * C)
   is
      L : constant Real := Ps * Qs;
      E : constant Real := Q * P + L - (Qs + Qs) * P;
   begin
      Lemma_Triple_Longest (P, Q, C, L);
      if Q >= Qs then
         Lemma_Monotone (P, P, Q, C);
         Lemma_Monotone (P, P, Qs, Q);
         pragma Assert (E >= Q * P + P * Q - (Qs + Qs) * P);
         pragma Assert (Q * P + P * Q - (Qs + Qs) * P = 2.0 * (P * Q - P * Qs));
      else
         Lemma_Nonnegative (Qs, Q * Ps - Qs * P);
         Lemma_Squares (Q, Qs);
         Lemma_Nonnegative (P, Q * Q + Qs * Qs - 2.0 * Q * Qs);
         pragma Assert (Q * E = Qs * (Q * Ps - Qs * P) + P * (Q * Q + Qs * Qs - 2.0 * Q * Qs));
         Lemma_Cancel (Q, E, 0.0);
      end if;
      pragma Assert (E >= 0.0);
      Lemma_Nonnegative (E, C);
      pragma Assert (E * C = (Q * P + L) * C - (Qs + Qs) * (P * C));
   end Lemma_Triple_Product_Bounded_Short;
   --  The star's third span is the sum of its first two.

   procedure Lemma_Triple_Sum_Bounded (P, Q, C, Ps, Qs, Cs : Real)
   with
     Pre  =>
       P > 0.0
       and then Q > 0.0
       and then C > 0.0
       and then Ps > 0.0
       and then Qs > 0.0
       and then Cs > 0.0
       and then Q <= C
       and then Q + C <= Qs + Cs
       and then Qs <= Cs
       and then (if Q >= Qs then P <= Ps else Q * Ps >= Qs * P),
     Post => (Q + C) * (Ps * Cs) >= (Qs + Cs) * (P * C)
   is
      Ss : constant Real := Qs + Cs;
      R  : constant Real := Ss - Q;
      --  The longest that C can be, taking the sum with it.
   begin
      pragma Assert (R >= C);
      Lemma_Nonnegative (Q, R - C);
      pragma Assert ((Q + C) * R - Ss * C = Q * (R - C));
      --  P * R is at most Ps * Cs.
      if Q >= Qs then
         Lemma_Monotone (P, Ps, R, Cs);
      else
         Lemma_Nonnegative (Q * Ps - Qs * P, R);
         pragma Assert (Ps * (Q * R) - Qs * (P * R) = (Q * Ps - Qs * P) * R);
         pragma Assert (Cs >= Q);
         Lemma_Nonnegative (Qs - Q, Cs - Q);
         pragma Assert (Qs * Cs - Q * R = (Qs - Q) * (Cs - Q));
         Lemma_Nonnegative (Ps, Qs * Cs - Q * R);
         pragma Assert (Qs * (Ps * Cs) - Ps * (Q * R) = Ps * (Qs * Cs - Q * R));
         Lemma_Cancel (Qs, Ps * Cs, P * R);
      end if;
      pragma Assert (P * R <= Ps * Cs);
      Lemma_Nonnegative (Ss, C);
      Lemma_Nonnegative (P, R);
      Lemma_Monotone (Ss * C, (Q + C) * R, P * R, Ps * Cs);
      pragma Assert (R * ((Q + C) * (Ps * Cs)) = ((Q + C) * R) * (Ps * Cs));
      pragma Assert (R * (Ss * (P * C)) = (Ss * C) * (P * R));
      Lemma_Cancel (R, (Q + C) * (Ps * Cs), Ss * (P * C));
   end Lemma_Triple_Sum_Bounded;

   procedure Lemma_Triple (A, B, C, As, Bs, Cs : Real)
   with
     Pre  =>
       A > 0.0
       and then B >= A
       and then C >= A + B
       and then As > 0.0
       and then Bs >= As
       and then Cs >= As + Bs
       and then (Bs = As or else A <= As)
       and then (Cs = As + Bs or else A * B <= As * Bs)
       and then (A * B * C <= As * Bs * Cs or else A + B + C <= As + Bs + Cs),
     Post => (A + B + C) * (As * Bs * Cs) >= (As + Bs + Cs) * (A * B * C)
   is
      P  : constant Real := A * B;
      Q  : constant Real := A + B;
      Ps : constant Real := As * Bs;
      Qs : constant Real := As + Bs;
   begin
      Lemma_Positive (A, B);
      Lemma_Positive (As, Bs);
      if P * C <= Ps * Cs then
         if P <= Ps then
            Lemma_Pair (A, B, As, Bs);
            Lemma_Triple_Product_Bounded (P, Q, C, Ps, Qs, Cs);
         else
            if Q < Qs then
               Lemma_Pair (A, B, As, Bs);
            end if;
            Lemma_Triple_Product_Bounded_Short (P, Q, C, Ps, Qs);
         end if;
      else
         if P <= Ps then
            Lemma_Pair (A, B, As, Bs);
         else
            --  Cs = Qs, so twice Q is at most Q + C, which is at most twice Qs.
            pragma Assert (Q <= Qs);
            Lemma_Pair (A, B, As, Bs);
            if Q >= Qs then
               Lemma_Cancel (Qs, Ps, P);
            end if;
         end if;
         Lemma_Triple_Sum_Bounded (P, Q, C, Ps, Qs, Cs);
      end if;
   end Lemma_Triple;

   ------------------------------------------------------------------------------------------------------------------
   --  Four spans: the sum is least for the star, where M and S are the product and the sum of the first three
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Quadruple_Product_Bounded (M, S, D, Ms, Ss, Ds : Real)
   with
     Pre  =>
       M > 0.0
       and then Ms > 0.0
       and then M <= Ms
       and then Ss >= 0.0
       and then Ss <= Ds
       and then S * Ms >= Ss * M
       and then M * D >= Ms * Ds,
     Post => S + D >= Ss + Ds
   is
      E : constant Real := M * (S - Ss) + Ds * (Ms - M);
   begin
      Lemma_Nonnegative (M, S * Ms - Ss * M);
      Lemma_Monotone (Ss, Ds, M, Ms);
      Lemma_Nonnegative (Ms - M, Ds * Ms - M * Ss);
      pragma Assert (Ms * E = M * (S * Ms - Ss * M) + (Ms - M) * (Ds * Ms - M * Ss));
      Lemma_Cancel (Ms, E, 0.0);
      pragma Assert (M * (S + D - Ss - Ds) = E + (M * D - Ms * Ds));
      Lemma_Cancel (M, S + D - Ss - Ds, 0.0);
   end Lemma_Quadruple_Product_Bounded;
   --  The star's fourth span is at least the sum of its first three, and its first three have the larger product.

   procedure Lemma_Quadruple_Sum_Bounded (M, S, D, Ms, Ss : Real)
   with
     Pre  =>
       M > 0.0
       and then Ms > 0.0
       and then S > 0.0
       and then Ss >= 0.0
       and then D >= 0.0
       and then S * Ms >= Ss * M
       and then M * D >= Ms * Ss,
     Post => S + D >= 2.0 * Ss
   is
   begin
      --  S * D is at least the square of Ss.
      Lemma_Nonnegative (Ss, M);
      Lemma_Monotone (Ss * M, S * Ms, D, D);
      Lemma_Nonnegative (Ms, Ss);
      Lemma_Monotone (Ss, Ss, Ms * Ss, M * D);
      pragma Assert (Ms * (S * D) = (S * Ms) * D);
      pragma Assert ((Ss * M) * D = Ss * (M * D));
      pragma Assert (Ms * (Ss * Ss) = Ss * (Ms * Ss));
      Lemma_Cancel (Ms, S * D, Ss * Ss);
      Lemma_Squares (S, Ss);
      pragma Assert (S * (S + D) = S * S + S * D);
      pragma Assert (S * (2.0 * Ss) = 2.0 * S * Ss);
      Lemma_Cancel (S, S + D, 2.0 * Ss);
   end Lemma_Quadruple_Sum_Bounded;
   --  The star's fourth span is the sum of its first three, which is the larger sum.

   procedure Lemma_Quadruple (A, B, C, D, As, Bs, Cs, Ds : Real)
   with
     Pre  =>
       A >= 0.0
       and then B >= A
       and then C >= A + B
       and then D >= A + B + C
       and then As > 0.0
       and then Bs >= As
       and then Cs >= As + Bs
       and then Ds >= As + Bs + Cs
       and then (Bs = As or else A <= As)
       and then (Cs = As + Bs or else A * B <= As * Bs)
       and then (Ds = As + Bs + Cs or else A * B * C <= As * Bs * Cs)
       and then A * B * C * D >= As * Bs * Cs * Ds,
     Post => A + B + C + D >= As + Bs + Cs + Ds
   is
      M  : constant Real := A * B * C;
      S  : constant Real := A + B + C;
      Ms : constant Real := As * Bs * Cs;
      Ss : constant Real := As + Bs + Cs;
   begin
      Lemma_Positive (As, Bs);
      Lemma_Positive (As * Bs, Cs);
      Lemma_Positive (Ms, Ds);
      --  The product of the star's spans is positive, so no span of the other ramp is zero.
      pragma Assert (if A = 0.0 then A * B * C * D = 0.0);
      pragma Assert (A > 0.0);
      Lemma_Positive (A, B);
      Lemma_Positive (A * B, C);
      if M <= Ms then
         Lemma_Triple (A, B, C, As, Bs, Cs);
         Lemma_Quadruple_Product_Bounded (M, S, D, Ms, Ss, Ds);
      elsif S < Ss then
         Lemma_Triple (A, B, C, As, Bs, Cs);
         Lemma_Quadruple_Sum_Bounded (M, S, D, Ms, Ss);
      end if;
   end Lemma_Quadruple;

   ------------------------------------------------------------------------------------------------------------------
   --  The change of variables
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Spans (R : Ramp) is
   begin
      null;
   end Lemma_Spans;

   procedure Lemma_Ordered_Spans (R : Ramp) is
   begin
      null;
   end Lemma_Ordered_Spans;

   procedure Lemma_Ramp_With_Spans (A, B, C, D, Crackle, Start : Real) is
   begin
      null;
   end Lemma_Ramp_With_Spans;

   ------------------------------------------------------------------------------------------------------------------
   --  One ramp against another
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Reduced_Crackle (K, F, Co, T1, B, C, D, As, Bs, Cs, Ds : Real)
   with
     Pre  => K > 0.0 and then Co = K * F,
     Post =>
       (if Co * T1 <= K * As then F * T1 <= As)
       and then (if Co * T1 * B <= K * As * Bs then (F * T1) * B <= As * Bs)
       and then (if Co * T1 * B * C <= K * As * Bs * Cs then (F * T1) * B * C <= As * Bs * Cs)
       and then (if Co * T1 * B * C * D >= K * As * Bs * Cs * Ds then (F * T1) * B * C * D >= As * Bs * Cs * Ds)
   is
   begin
      pragma Assert (Co * T1 = K * (F * T1));
      if Co * T1 <= K * As then
         Lemma_Cancel (K, As, F * T1);
      end if;
      pragma Assert (Co * T1 * B = K * ((F * T1) * B));
      pragma Assert (K * As * Bs = K * (As * Bs));
      if Co * T1 * B <= K * As * Bs then
         Lemma_Cancel (K, As * Bs, (F * T1) * B);
      end if;
      pragma Assert (Co * T1 * B * C = K * ((F * T1) * B * C));
      pragma Assert (K * As * Bs * Cs = K * (As * Bs * Cs));
      if Co * T1 * B * C <= K * As * Bs * Cs then
         Lemma_Cancel (K, As * Bs * Cs, (F * T1) * B * C);
      end if;
      pragma Assert (Co * T1 * B * C * D = K * ((F * T1) * B * C * D));
      pragma Assert (K * As * Bs * Cs * Ds = K * (As * Bs * Cs * Ds));
      if Co * T1 * B * C * D >= K * As * Bs * Cs * Ds then
         Lemma_Cancel (K, (F * T1) * B * C * D, As * Bs * Cs * Ds);
      end if;
   end Lemma_Reduced_Crackle;
   --  A ramp with crackle Co, a fraction F of K, has the peaks and the change of speed of one with crackle K whose
   --  first span is F times as long. Comparisons with a ramp of crackle K then compare products of spans.

   procedure Lemma_No_Faster_Ramp (Star, Other : Ramp) is
      K  : constant Real := Star.C;
      As : constant Real := Snap_Span (Star);
      Bs : constant Real := Jerk_Span (Star);
      Cs : constant Real := Acceleration_Span (Star);
      Ds : constant Real := Velocity_Span (Star);
      F  : constant Real := Other.C / K;
      A  : constant Real := F * Other.T1;
      B  : constant Real := Jerk_Span (Other);
      C  : constant Real := Acceleration_Span (Other);
      D  : constant Real := Velocity_Span (Other);
   begin
      Lemma_Fraction (Other.C, K);
      pragma Assert (Other.C = K * F);
      pragma Assert (F >= 0.0 and then F <= 1.0);
      Lemma_Nonnegative (F, Other.T1);
      Lemma_Monotone (F, 1.0, Other.T1, Other.T1);
      pragma Assert (A >= 0.0 and then A <= Other.T1);
      Lemma_Reduced_Crackle (K, F, Other.C, Other.T1, B, C, D, As, Bs, Cs, Ds);
      pragma Assert (Peak_Snap (Other) = Other.C * Other.T1);
      pragma Assert (Peak_Jerk (Other) = Other.C * Other.T1 * B);
      pragma Assert (Peak_Accel (Other) = Other.C * Other.T1 * B * C);
      pragma Assert (Velocity_Gain (Other) = Other.C * Other.T1 * B * C * D);
      pragma Assert (Peak_Snap (Star) = K * As);
      pragma Assert (Peak_Jerk (Star) = K * As * Bs);
      pragma Assert (Peak_Accel (Star) = K * As * Bs * Cs);
      pragma Assert (Velocity_Gain (Star) = K * As * Bs * Cs * Ds);
      pragma Assert (Bs = As or else A <= As);
      pragma Assert (Cs = As + Bs or else A * B <= As * Bs);
      pragma Assert (Ds = As + Bs + Cs or else A * B * C <= As * Bs * Cs);
      pragma Assert (A * B * C * D >= As * Bs * Cs * Ds);
      Lemma_Quadruple (A, B, C, D, As, Bs, Cs, Ds);
      pragma Assert (Total_Time (Other) = Other.T1 + B + C + D);
      pragma Assert (Total_Time (Star) = As + Bs + Cs + Ds);
   end Lemma_No_Faster_Ramp;

   ------------------------------------------------------------------------------------------------------------------
   --  Ramps of either direction
   ------------------------------------------------------------------------------------------------------------------

   function Magnitude (R : Ramp) return Ramp
   is ((T1 => R.T1, T2 => R.T2, T3 => R.T3, T4 => R.T4, C => abs R.C, Vs => R.Vs));
   --  The ramp with the same phase times which speeds up.

   procedure Lemma_Abs_Scaled (X, Y : Real) with Pre => Y >= 0.0, Post => abs (X * Y) = abs X * Y is
   begin
      null;
   end Lemma_Abs_Scaled;

   procedure Lemma_Abs_Products (C, X1, X2, X3, X4 : Real)
   with
     Pre  => X1 >= 0.0 and then X2 >= 0.0 and then X3 >= 0.0 and then X4 >= 0.0,
     Post =>
       abs (C * X1) = abs C * X1
       and then abs (C * X1 * X2) = abs C * X1 * X2
       and then abs (C * X1 * X2 * X3) = abs C * X1 * X2 * X3
       and then abs (C * X1 * X2 * X3 * X4) = abs C * X1 * X2 * X3 * X4
   is
   begin
      Lemma_Abs_Scaled (C, X1);
      Lemma_Abs_Scaled (C * X1, X2);
      Lemma_Abs_Scaled (C * X1 * X2, X3);
      Lemma_Abs_Scaled (C * X1 * X2 * X3, X4);
   end Lemma_Abs_Products;

   procedure Lemma_Magnitude (R : Ramp)
   with
     Pre  => Is_Ramp (R),
     Post =>
       Is_Ramp (Magnitude (R))
       and then Magnitude (R).C = abs R.C
       and then Magnitude (R).T1 = R.T1
       and then Magnitude (R).T2 = R.T2
       and then Magnitude (R).T3 = R.T3
       and then Magnitude (R).T4 = R.T4
       and then Peak_Snap (Magnitude (R)) = abs Peak_Snap (R)
       and then Peak_Jerk (Magnitude (R)) = abs Peak_Jerk (R)
       and then Peak_Accel (Magnitude (R)) = abs Peak_Accel (R)
       and then Velocity_Gain (Magnitude (R)) = abs Velocity_Gain (R)
       and then Total_Time (Magnitude (R)) = Total_Time (R)
   is
   begin
      Lemma_Abs_Products (R.C, R.T1, R.T1 + R.T2, 2.0 * R.T1 + R.T2 + R.T3, 4.0 * R.T1 + 2.0 * R.T2 + R.T3 + R.T4);
   end Lemma_Magnitude;

   procedure Lemma_Saturating_Is_Fastest (Star, Other : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   is
   begin
      Lemma_Magnitude (Star);
      Lemma_Magnitude (Other);
      if Star.T1 = 0.0 then
         --  With no first phase every peak is zero, so no limit is reached and the ramp is empty.
         pragma Assert (Peak_Snap (Star) = 0.0);
         pragma Assert (Star.T2 = 0.0);
         pragma Assert (Peak_Jerk (Star) = 0.0);
         pragma Assert (Star.T3 = 0.0);
         pragma Assert (Peak_Accel (Star) = 0.0);
         pragma Assert (Star.T4 = 0.0);
         pragma Assert (Total_Time (Star) = 0.0);
         pragma Assert (Total_Time (Other) >= 0.0);
      else
         Lemma_No_Faster_Ramp (Magnitude (Star), Magnitude (Other));
      end if;
   end Lemma_Saturating_Is_Fastest;

   procedure Lemma_Saturating_Times_Agree
     (Star_1, Star_2 : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real) is
   begin
      Lemma_Saturating_Is_Fastest (Star_1, Star_2, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
      Lemma_Saturating_Is_Fastest (Star_2, Star_1, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
   end Lemma_Saturating_Times_Agree;

   ------------------------------------------------------------------------------------------------------------------
   --  A fixed duration
   ------------------------------------------------------------------------------------------------------------------

   function Shrunk (R : Ramp; F : Real) return Ramp
   is ((T1 => F * R.T1, T2 => F * R.T2, T3 => F * R.T3, T4 => F * R.T4, C => R.C, Vs => R.Vs));
   --  The ramp with every phase time F times as long.

   procedure Lemma_Shrunk_Products (C, F, T1, T2, T3, T4 : Real)
   with
     Post =>
       C * (F * T1) = F * (C * T1)
       and then C * (F * T1) * (F * T1 + F * T2) = (F * F) * (C * T1 * (T1 + T2))
       and then
         C * (F * T1) * (F * T1 + F * T2) * (2.0 * (F * T1) + F * T2 + F * T3)
         = (F * F * F) * (C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3))
       and then
         C
         * (F * T1)
         * (F * T1 + F * T2)
         * (2.0 * (F * T1) + F * T2 + F * T3)
         * (4.0 * (F * T1) + 2.0 * (F * T2) + F * T3 + F * T4)
         = (F * F * F * F) * (C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4))
       and then 8.0 * (F * T1) + 4.0 * (F * T2) + 2.0 * (F * T3) + F * T4 = F * (8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4)
   is
   begin
      null;
   end Lemma_Shrunk_Products;

   procedure Lemma_Fraction_Of (F, X : Real)
   with Pre => F >= 0.0 and then F <= 1.0, Post => abs (F * X) = F * abs X and then abs (F * X) <= abs X
   is
   begin
      Lemma_Monotone (F, 1.0, abs X, abs X);
   end Lemma_Fraction_Of;

   procedure Lemma_Shrunk (R : Ramp; F : Real)
   with
     Pre  => Is_Ramp (R) and then F >= 0.0 and then F <= 1.0,
     Post =>
       Is_Ramp (Shrunk (R, F))
       and then Shrunk (R, F).C = R.C
       and then abs Peak_Snap (Shrunk (R, F)) <= abs Peak_Snap (R)
       and then abs Peak_Jerk (Shrunk (R, F)) <= abs Peak_Jerk (R)
       and then abs Peak_Accel (Shrunk (R, F)) <= abs Peak_Accel (R)
       and then abs Velocity_Gain (Shrunk (R, F)) = (F * F * F * F) * abs Velocity_Gain (R)
       and then Total_Time (Shrunk (R, F)) = F * Total_Time (R)
   is
   begin
      Lemma_Nonnegative (F, R.T1);
      Lemma_Nonnegative (F, R.T2);
      Lemma_Nonnegative (F, R.T3);
      Lemma_Nonnegative (F, R.T4);
      Lemma_Shrunk_Products (R.C, F, R.T1, R.T2, R.T3, R.T4);
      Lemma_Monotone (F, 1.0, F, 1.0);
      Lemma_Monotone (F * F, 1.0, F, 1.0);
      Lemma_Monotone (F * F * F, 1.0, F, 1.0);
      Lemma_Fraction_Of (F, Peak_Snap (R));
      Lemma_Fraction_Of (F * F, Peak_Jerk (R));
      Lemma_Fraction_Of (F * F * F, Peak_Accel (R));
      Lemma_Fraction_Of (F * F * F * F, Velocity_Gain (R));
   end Lemma_Shrunk;

   procedure Lemma_Fourth_Power (E : Real)
   with Pre => E >= 0.0 and then E <= 0.25, Post => (1.0 - E) * (1.0 - E) * (1.0 - E) * (1.0 - E) >= 1.0 - 4.0 * E
   is
      F : constant Real := 1.0 - E;
   begin
      Lemma_Nonnegative (E, E);
      pragma Assert (F * F = 1.0 - 2.0 * E + E * E);
      pragma Assert (F * F >= 1.0 - 2.0 * E);
      Lemma_Monotone (1.0 - 2.0 * E, F * F, 1.0 - 2.0 * E, F * F);
      pragma Assert ((1.0 - 2.0 * E) * (1.0 - 2.0 * E) = 1.0 - 4.0 * E + 4.0 * (E * E));
      pragma Assert (F * F * F * F = (F * F) * (F * F));
   end Lemma_Fourth_Power;

   procedure Lemma_Positive_Gain_Takes_Time (R : Ramp)
   with Pre => Is_Ramp (R) and then abs Velocity_Gain (R) > 0.0, Post => Total_Time (R) > 0.0
   is
   begin
      pragma Assert (if R.T1 = 0.0 then Velocity_Gain (R) = 0.0);
   end Lemma_Positive_Gain_Takes_Time;

   procedure Lemma_Saturating_Gains_Most (Star, Other : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   is
      Go : constant Real := abs Velocity_Gain (Other);
      Gs : constant Real := abs Velocity_Gain (Star);
   begin
      if Go > Gs then
         --  Shrink the other ramp until it changes the speed by only as much as the star. It is still within the
         --  limits, and takes less time than it did, so less than the star: which the theorem above rules out.
         declare
            Ratio  : constant Real := Gs / Go;
            E      : constant Real := (1.0 - Ratio) / 4.0;
            F      : constant Real := 1.0 - E;
            Faster : constant Ramp := Shrunk (Other, F);
         begin
            Lemma_Fraction (Gs, Go);
            pragma Assert (Go * Ratio = Gs);
            pragma Assert (Ratio >= 0.0);
            if Ratio >= 1.0 then
               Lemma_Monotone (1.0, Ratio, Go, Go);
               pragma Assert (Ratio * Go = Go * Ratio);
            end if;
            pragma Assert (Ratio < 1.0);
            pragma Assert (E > 0.0 and then E <= 0.25);
            Lemma_Shrunk (Other, F);
            Lemma_Fourth_Power (E);
            pragma Assert (F * F * F * F >= Ratio);
            Lemma_Monotone (Ratio, F * F * F * F, Go, Go);
            pragma Assert (Ratio * Go = Go * Ratio);
            pragma Assert (abs Velocity_Gain (Faster) >= Gs);
            Lemma_Saturating_Is_Fastest (Star, Faster, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
            Lemma_Positive_Gain_Takes_Time (Other);
            Lemma_Positive (E, Total_Time (Other));
            pragma Assert (Total_Time (Other) - F * Total_Time (Other) = E * Total_Time (Other));
            pragma Assert (Total_Time (Faster) < Total_Time (Other));
            pragma Assert (False);
         end;
      end if;
   end Lemma_Saturating_Gains_Most;

   ------------------------------------------------------------------------------------------------------------------
   --  Distance
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Nonnegative_Gain (R : Ramp)
   with Pre => Is_Ramp (R) and then R.C >= 0.0, Post => Velocity_Gain (R) >= 0.0 and then Total_Time (R) >= 0.0
   is
   begin
      Lemma_Nonnegative (R.C, R.T1);
      Lemma_Nonnegative (R.C * R.T1, R.T1 + R.T2);
      Lemma_Nonnegative (R.C * R.T1 * (R.T1 + R.T2), 2.0 * R.T1 + R.T2 + R.T3);
      Lemma_Nonnegative
        (R.C * R.T1 * (R.T1 + R.T2) * (2.0 * R.T1 + R.T2 + R.T3), 4.0 * R.T1 + 2.0 * R.T2 + R.T3 + R.T4);
   end Lemma_Nonnegative_Gain;

   procedure Lemma_Travel (R : Ramp; V, G, T : Real)
   with
     Pre  => R.Vs = V and then Velocity_Gain (R) = G and then Total_Time (R) = T,
     Post => Travel (R) = (V + G / 2.0) * T
   is
   begin
      null;
   end Lemma_Travel;

   procedure Lemma_Saturating_Is_Fastest_Over_Distance
     (Star, Other : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   is
      V  : constant Real := Star.Vs;
      Go : constant Real := Velocity_Gain (Other);
      Gs : constant Real := Velocity_Gain (Star);
      To : constant Real := Total_Time (Other);
      Ts : constant Real := Total_Time (Star);
   begin
      Lemma_Nonnegative_Gain (Star);
      Lemma_Nonnegative_Gain (Other);
      Lemma_Travel (Star, V, Gs, Ts);
      Lemma_Travel (Other, V, Go, To);
      if To < Ts then
         if Go >= Gs then
            Lemma_Saturating_Is_Fastest (Star, Other, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
         else
            --  Less change of speed for less time is less distance.
            Lemma_Monotone (V + Go / 2.0, V + Gs / 2.0, To, To);
            Lemma_Positive (V + Gs / 2.0, Ts - To);
            pragma Assert ((V + Gs / 2.0) * Ts - (V + Gs / 2.0) * To = (V + Gs / 2.0) * (Ts - To));
            pragma Assert (Travel (Other) < Travel (Star));
         end if;
         pragma Assert (False);
      end if;
   end Lemma_Saturating_Is_Fastest_Over_Distance;

   procedure Lemma_Saturating_Travels_Farthest
     (Star, Other : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   is
      V  : constant Real := Star.Vs;
      Go : constant Real := Velocity_Gain (Other);
      Gs : constant Real := Velocity_Gain (Star);
      To : constant Real := Total_Time (Other);
      Ts : constant Real := Total_Time (Star);
   begin
      Lemma_Nonnegative_Gain (Star);
      Lemma_Nonnegative_Gain (Other);
      Lemma_Travel (Star, V, Gs, Ts);
      Lemma_Travel (Other, V, Go, To);
      Lemma_Saturating_Gains_Most (Star, Other, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
      pragma Assert (Go <= Gs);
      Lemma_Monotone (V + Go / 2.0, V + Gs / 2.0, To, Ts);
   end Lemma_Saturating_Travels_Farthest;

   ------------------------------------------------------------------------------------------------------------------
   --  The solvers, in exact arithmetic
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Strict (X1, X2, Y1, Y2 : Real)
   with
     Pre  => X1 >= 0.0 and then X1 <= X2 and then X2 > 0.0 and then Y1 >= 0.0 and then Y1 < Y2,
     Post => X1 * Y1 < X2 * Y2
   is
   begin
      Lemma_Monotone (X1, X2, Y1, Y1);
      Lemma_Positive (X2, Y2 - Y1);
      pragma Assert (X2 * Y2 - X2 * Y1 = X2 * (Y2 - Y1));
   end Lemma_Strict;

   procedure Lemma_Peaks_Monotone (K, T1, T2, T3, U1, U2, U3 : Real)
   with
     Pre  =>
       K >= 0.0
       and then T1 >= 0.0
       and then T1 <= U1
       and then T2 >= 0.0
       and then T2 <= U2
       and then T3 >= 0.0
       and then T3 <= U3,
     Post =>
       K * T1 <= K * U1
       and then K * T1 * (T1 + T2) <= K * U1 * (U1 + U2)
       and then K * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) <= K * U1 * (U1 + U2) * (2.0 * U1 + U2 + U3)
       and then K * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) >= 0.0
   is
   begin
      Lemma_Monotone (K, K, T1, U1);
      Lemma_Nonnegative (K, T1);
      Lemma_Monotone (K * T1, K * U1, T1 + T2, U1 + U2);
      Lemma_Nonnegative (K * T1, T1 + T2);
      Lemma_Monotone (K * T1 * (T1 + T2), K * U1 * (U1 + U2), 2.0 * T1 + T2 + T3, 2.0 * U1 + U2 + U3);
      Lemma_Nonnegative (K * T1 * (T1 + T2), 2.0 * T1 + T2 + T3);
   end Lemma_Peaks_Monotone;
   --  The peaks grow with each of the first three phase times.

   procedure Lemma_Longer_Gains_More (K, T1, T2, T3, T4, U1, U2, U3, U4 : Real)
   with
     Pre  =>
       K > 0.0
       and then T1 >= 0.0
       and then T1 <= U1
       and then U1 > 0.0
       and then T2 >= 0.0
       and then T2 <= U2
       and then T3 >= 0.0
       and then T3 <= U3
       and then T4 >= 0.0
       and then 4.0 * T1 + 2.0 * T2 + T3 + T4 < 4.0 * U1 + 2.0 * U2 + U3 + U4,
     Post =>
       K * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4)
       < K * U1 * (U1 + U2) * (2.0 * U1 + U2 + U3) * (4.0 * U1 + 2.0 * U2 + U3 + U4)
   is
   begin
      Lemma_Peaks_Monotone (K, T1, T2, T3, U1, U2, U3);
      Lemma_Positive (K, U1);
      Lemma_Positive (K * U1, U1 + U2);
      Lemma_Positive (K * U1 * (U1 + U2), 2.0 * U1 + U2 + U3);
      Lemma_Strict
        (K * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3),
         K * U1 * (U1 + U2) * (2.0 * U1 + U2 + U3),
         4.0 * T1 + 2.0 * T2 + T3 + T4,
         4.0 * U1 + 2.0 * U2 + U3 + U4);
   end Lemma_Longer_Gains_More;
   --  A ramp with no shorter a phase, and a longer velocity span, changes the speed by more.

   procedure Lemma_Abs_Peaks (R : Ramp; K, T1, T2, T3, T4 : Real)
   with
     Pre  =>
       Is_Ramp (R) and then K = abs R.C and then T1 = R.T1 and then T2 = R.T2 and then T3 = R.T3 and then T4 = R.T4,
     Post =>
       abs Peak_Snap (R) = K * T1
       and then abs Peak_Jerk (R) = K * T1 * (T1 + T2)
       and then abs Peak_Accel (R) = K * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3)
       and then abs Velocity_Gain (R) = K * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4)
       and then Total_Time (R) = 8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4
   is
   begin
      Lemma_Abs_Products (R.C, T1, T1 + T2, 2.0 * T1 + T2 + T3, 4.0 * T1 + 2.0 * T2 + T3 + T4);
      pragma Assert (Peak_Snap (R) = R.C * T1);
      pragma Assert (Peak_Jerk (R) = R.C * T1 * (T1 + T2));
      pragma Assert (Peak_Accel (R) = R.C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3));
      pragma
        Assert (Velocity_Gain (R) = R.C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4));
   end Lemma_Abs_Peaks;
   --  The magnitudes of the peaks and of the change of speed of a ramp, in terms of what its components are.

   procedure Lemma_Cut_Short_Is_Saturating
     (Full : Ramp; Last : Phase_Index; Time : Real; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   is
      R  : constant Ramp := Cut_Short (Full, Last, Time);
      K  : constant Real := abs Full.C;
      T1 : constant Real := R.T1;
      T2 : constant Real := R.T2;
      T3 : constant Real := R.T3;
      T4 : constant Real := R.T4;
      U1 : constant Real := Full.T1;
      U2 : constant Real := Full.T2;
      U3 : constant Real := Full.T3;
      U4 : constant Real := Full.T4;
   begin
      pragma Assert (Is_Ramp (R));
      pragma Assert (R.C = Full.C);
      pragma Assert (T1 <= U1 and then T2 <= U2 and then T3 <= U3);
      Lemma_Abs_Peaks (Full, K, U1, U2, U3, U4);
      Lemma_Abs_Peaks (R, K, T1, T2, T3, T4);
      Lemma_Peaks_Monotone (K, T1, T2, T3, U1, U2, U3);
      pragma Assert (abs Peak_Snap (R) <= abs Peak_Snap (Full));
      pragma Assert (abs Peak_Jerk (R) <= abs Peak_Jerk (Full));
      pragma Assert (abs Peak_Accel (R) <= abs Peak_Accel (Full));
      pragma Assert (Is_Within_Limits (R, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max));
      --  A phase of the ramp cut short which is not empty is not empty in the full ramp either, the limit below
      --  it is reached there, and the two ramps are the same up to that phase.
      case Last is
         when 1 =>
            null;

         when 2 =>
            pragma Assert (Peak_Snap (R) = Peak_Snap (Full));
            pragma Assert (if R.T2 /= 0.0 then Full.T2 /= 0.0);

         when 3 =>
            pragma Assert (Peak_Snap (R) = Peak_Snap (Full));
            pragma Assert (Peak_Jerk (R) = Peak_Jerk (Full));
            pragma Assert (if R.T3 /= 0.0 then Full.T3 /= 0.0);

         when 4 =>
            pragma Assert (Peak_Snap (R) = Peak_Snap (Full));
            pragma Assert (Peak_Jerk (R) = Peak_Jerk (Full));
            pragma Assert (Peak_Accel (R) = Peak_Accel (Full));
      end case;
   end Lemma_Cut_Short_Is_Saturating;

   procedure Lemma_Longer_Cut (Full : Ramp; Last : Phase_Index; Time : Real)
   with
     Pre  =>
       Is_Ramp (Full)
       and then Full.C /= 0.0
       and then Full.T1 > 0.0
       and then Last <= 3
       and then Time > Phase_Time (Full, Last),
     Post =>
       abs Velocity_Gain (Cut_Short (Full, Last, Time))
       > abs Velocity_Gain (Cut_Short (Full, Last, Phase_Time (Full, Last)))
       and then
         Total_Time (Cut_Short (Full, Last, Time)) > Total_Time (Cut_Short (Full, Last, Phase_Time (Full, Last)))
       and then Total_Time (Cut_Short (Full, Last, Phase_Time (Full, Last))) >= 0.0
   is
      Short : constant Ramp := Cut_Short (Full, Last, Phase_Time (Full, Last));
      Long  : constant Ramp := Cut_Short (Full, Last, Time);
      K     : constant Real := abs Full.C;
      T1    : constant Real := Short.T1;
      T2    : constant Real := Short.T2;
      T3    : constant Real := Short.T3;
      T4    : constant Real := Short.T4;
      U1    : constant Real := Long.T1;
      U2    : constant Real := Long.T2;
      U3    : constant Real := Long.T3;
      U4    : constant Real := Long.T4;
   begin
      pragma Assert (Is_Ramp (Short) and then Is_Ramp (Long));
      pragma Assert (Short.C = Full.C and then Long.C = Full.C);
      pragma Assert (T1 <= U1 and then T2 <= U2 and then T3 <= U3);
      pragma Assert (U1 > 0.0);
      pragma Assert (4.0 * T1 + 2.0 * T2 + T3 + T4 < 4.0 * U1 + 2.0 * U2 + U3 + U4);
      pragma Assert (8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 < 8.0 * U1 + 4.0 * U2 + 2.0 * U3 + U4);
      Lemma_Abs_Peaks (Short, K, T1, T2, T3, T4);
      Lemma_Abs_Peaks (Long, K, U1, U2, U3, U4);
      Lemma_Longer_Gains_More (K, T1, T2, T3, T4, U1, U2, U3, U4);
   end Lemma_Longer_Cut;
   --  A longer last phase gives a larger change of speed and a longer ramp.

   procedure Lemma_Gain_Selects_Phase (Full : Ramp; Last : Phase_Index; Time : Real) is
   begin
      if Time > Phase_Time (Full, Last) then
         Lemma_Longer_Cut (Full, Last, Time);
      end if;
   end Lemma_Gain_Selects_Phase;

   procedure Lemma_Travel_Selects_Phase (Full : Ramp; Last : Phase_Index; Time : Real) is
      Short : constant Ramp := Cut_Short (Full, Last, Phase_Time (Full, Last));
      Long  : constant Ramp := Cut_Short (Full, Last, Time);
      V     : constant Real := Full.Vs;
      Gs    : constant Real := Velocity_Gain (Short);
      Gl    : constant Real := Velocity_Gain (Long);
      Ts    : constant Real := Total_Time (Short);
      Tl    : constant Real := Total_Time (Long);
   begin
      if Time > Phase_Time (Full, Last) then
         pragma Assert (Is_Ramp (Short) and then Is_Ramp (Long));
         pragma Assert (Short.C >= 0.0 and then Long.C >= 0.0);
         pragma Assert (Short.Vs = V and then Long.Vs = V);
         Lemma_Longer_Cut (Full, Last, Time);
         Lemma_Nonnegative_Gain (Short);
         Lemma_Nonnegative_Gain (Long);
         pragma Assert (Gs < Gl);
         Lemma_Travel (Short, V, Gs, Ts);
         Lemma_Travel (Long, V, Gl, Tl);
         Lemma_Strict (V + Gs / 2.0, V + Gl / 2.0, Ts, Tl);
         pragma Assert (Travel (Short) < Travel (Long));
      end if;
   end Lemma_Travel_Selects_Phase;

   procedure Lemma_Fully_Saturating_Moves (Full : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Fully_Saturating (Full, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max),
     Post => Full.C /= 0.0 and then Full.T1 > 0.0
   is
   begin
      pragma Assert (if Full.T1 = 0.0 then Peak_Accel (Full) = 0.0);
   end Lemma_Fully_Saturating_Moves;
   --  A ramp which reaches a positive acceleration limit has a first phase.

   procedure Lemma_Delta_V_Case_Is_Fastest
     (Full                                              : Ramp;
      Last                                              : Phase_Index;
      Time                                              : Real;
      Other                                             : Ramp;
      Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real) is
   begin
      Lemma_Fully_Saturating_Moves (Full, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
      if Last /= 4 then
         Lemma_Gain_Selects_Phase (Full, Last, Time);
      end if;
      Lemma_Cut_Short_Is_Saturating (Full, Last, Time, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
      Lemma_Saturating_Is_Fastest
        (Cut_Short (Full, Last, Time), Other, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
   end Lemma_Delta_V_Case_Is_Fastest;

   procedure Lemma_Distance_Case_Is_Fastest
     (Full                                              : Ramp;
      Last                                              : Phase_Index;
      Time                                              : Real;
      Other                                             : Ramp;
      Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real) is
   begin
      Lemma_Fully_Saturating_Moves (Full, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
      if Last /= 4 then
         Lemma_Travel_Selects_Phase (Full, Last, Time);
      end if;
      Lemma_Cut_Short_Is_Saturating (Full, Last, Time, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
      pragma Assert (Cut_Short (Full, Last, Time).C = Full.C and then Cut_Short (Full, Last, Time).Vs = Full.Vs);
      Lemma_Saturating_Is_Fastest_Over_Distance
        (Cut_Short (Full, Last, Time), Other, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
   end Lemma_Distance_Case_Is_Fastest;

   ------------------------------------------------------------------------------------------------------------------
   --  The ramp of each region
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Square_Order (X, Y : Real)
   with Pre => X >= 0.0 and then Y >= 0.0 and then X * X <= Y * Y, Post => X <= Y
   is
   begin
      if X > Y then
         Lemma_Strict (Y, X, Y, X);
      end if;
   end Lemma_Square_Order;

   procedure Lemma_Cube_Order (X, Y : Real)
   with Pre => X >= 0.0 and then Y >= 0.0 and then X * X * X <= Y * Y * Y, Post => X <= Y
   is
   begin
      if X > Y then
         Lemma_Strict (Y, X, Y, X);
         Lemma_Nonnegative (Y, Y);
         Lemma_Strict (Y * Y, X * X, Y, X);
      end if;
   end Lemma_Cube_Order;

   procedure Lemma_Quotient (X, K, Q : Real) with Pre => K > 0.0 and then K * Q = X, Post => Q = X / K is
   begin
      pragma Assert (K * (X / K) = X);
      if Q > X / K then
         Lemma_Positive (K, Q - X / K);
         pragma Assert (K * (Q - X / K) = K * Q - K * (X / K));
      elsif Q < X / K then
         Lemma_Positive (K, X / K - Q);
         pragma Assert (K * (X / K - Q) = K * (X / K) - K * Q);
      end if;
   end Lemma_Quotient;

   procedure Lemma_Region_1_Peaks (Cm, Sm, Jm, Am : Real)
   with
     Pre  => Cm > 0.0 and then Sm > 0.0 and then Jm > 0.0,
     Post =>
       Cm * (Sm / Cm) = Sm
       and then Cm * (Sm / Cm) * (Sm / Cm + (Jm / Sm - Sm / Cm)) = Jm
       and then
         Cm
         * (Sm / Cm)
         * (Sm / Cm + (Jm / Sm - Sm / Cm))
         * (2.0 * (Sm / Cm) + (Jm / Sm - Sm / Cm) + (Am / Jm - Jm / Sm - Sm / Cm))
         = Am
       and then Sm / Cm >= 0.0
   is
   begin
      pragma Assert (Cm * (Sm / Cm) = Sm);
      pragma Assert (Sm / Cm + (Jm / Sm - Sm / Cm) = Jm / Sm);
      pragma Assert (Sm * (Jm / Sm) = Jm);
      pragma Assert (2.0 * (Sm / Cm) + (Jm / Sm - Sm / Cm) + (Am / Jm - Jm / Sm - Sm / Cm) = Am / Jm);
      pragma Assert (Jm * (Am / Jm) = Am);
      if Sm / Cm < 0.0 then
         Lemma_Positive (Cm, 0.0 - Sm / Cm);
         pragma Assert (Cm * (0.0 - Sm / Cm) = 0.0 - Cm * (Sm / Cm));
      end if;
   end Lemma_Region_1_Peaks;

   procedure Lemma_Region_1_Is_Saturating (Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max, T4, Start : Real) is
   begin
      Lemma_Region_1_Peaks (Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
   end Lemma_Region_1_Is_Saturating;

   procedure Lemma_Region_2_Jerk (Sm, Jm, Am, A, B, G : Real)
   with
     Pre  =>
       Sm > 0.0
       and then Jm > 0.0
       and then A >= 0.0
       and then B >= 0.0
       and then Sm * G = Jm
       and then Sm * B * (A + B) = Am
       and then Am <= Jm * (G + A),
     Post => Sm * B <= Jm
   is
   begin
      if Sm * B > Jm then
         if B <= G then
            Lemma_Monotone (Sm, Sm, B, G);
         end if;
         pragma Assert (B > G);
         if G < 0.0 then
            Lemma_Positive (Sm, 0.0 - G);
            pragma Assert (Sm * (0.0 - G) = 0.0 - Sm * G);
         end if;
         pragma Assert (G >= 0.0);
         Lemma_Strict (Jm, Sm * B, G + A, A + B);
      end if;
   end Lemma_Region_2_Jerk;
   --  If jerk went over its limit, the second span would be longer than in Region_1, and acceleration would go
   --  over the limit which separates the two regions.

   procedure Lemma_Region_2_Is_Saturating (R : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real) is
      A : constant Real := R.T1;
      B : constant Real := R.T1 + R.T2;
      G : constant Real := Jerk_Max / Snap_Max;
   begin
      Lemma_Quotient (Snap_Max, Crackle_Max, A);
      pragma Assert (Snap_Max * G = Jerk_Max);
      pragma Assert (Peak_Jerk (R) = Snap_Max * B);
      pragma Assert (Peak_Accel (R) = Snap_Max * B * (2.0 * R.T1 + R.T2 + R.T3));
      pragma Assert (2.0 * R.T1 + R.T2 + R.T3 = A + B);
      Lemma_Region_2_Jerk (Snap_Max, Jerk_Max, Acceleration_Max, A, B, G);
      Lemma_Nonnegative (Snap_Max, B);
   end Lemma_Region_2_Is_Saturating;

   procedure Lemma_Region_3_Peaks (Cm, Sm, Jm, Am, T : Real)
   with
     Pre  =>
       Cm > 0.0
       and then Sm > 0.0
       and then Jm > 0.0
       and then T >= 0.0
       and then Cm * T * (T + 0.0) * (2.0 * T + 0.0 + 0.0) = Am
       and then Sm * Sm <= Jm * Cm
       and then Am * Cm * Cm <= 2.0 * Sm * Sm * Sm,
     Post => Cm * T >= 0.0 and then Cm * T <= Sm and then Cm * T * (T + 0.0) >= 0.0 and then Cm * T * (T + 0.0) <= Jm
   is
      X : constant Real := Cm * T;
   begin
      Lemma_Nonnegative (Cm, T);
      Lemma_Nonnegative (X, T);
      pragma Assert (2.0 * (X * X * X) = (Cm * T * (T + 0.0) * (2.0 * T + 0.0 + 0.0)) * Cm * Cm);
      pragma Assert (2.0 * (X * X * X) <= 2.0 * (Sm * Sm * Sm));
      Lemma_Cube_Order (X, Sm);
      Lemma_Monotone (X, Sm, X, Sm);
      pragma Assert (Cm * (Cm * T * (T + 0.0)) = X * X);
      pragma Assert (Cm * Jm = Jm * Cm);
      Lemma_Cancel (Cm, Jm, Cm * T * (T + 0.0));
   end Lemma_Region_3_Peaks;

   procedure Lemma_Region_3_Is_Saturating (R : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real) is
   begin
      Lemma_Region_3_Peaks (Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max, R.T1);
   end Lemma_Region_3_Is_Saturating;

   procedure Lemma_Region_4_Snap (Cm, Sm, Jm, T : Real)
   with
     Pre  => Cm > 0.0 and then Sm > 0.0 and then T >= 0.0 and then Cm * T * (T + 0.0) = Jm and then Jm * Cm <= Sm * Sm,
     Post => Cm * T >= 0.0 and then Cm * T <= Sm
   is
      X : constant Real := Cm * T;
   begin
      Lemma_Nonnegative (Cm, T);
      pragma Assert (X * X = (Cm * T * (T + 0.0)) * Cm);
      Lemma_Square_Order (X, Sm);
   end Lemma_Region_4_Snap;

   procedure Lemma_Region_4_Is_Saturating (R : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real) is
   begin
      Lemma_Region_4_Snap (Crackle_Max, Snap_Max, Jerk_Max, R.T1);
   end Lemma_Region_4_Is_Saturating;

   procedure Lemma_Region_5_Peaks (Cm, Sm, Jm, Am, T : Real)
   with
     Pre  =>
       Cm > 0.0
       and then Sm > 0.0
       and then Jm > 0.0
       and then T >= 0.0
       and then Cm * T * (T + 0.0) * (2.0 * T + 0.0 + 0.0) = Am
       and then Jm * Cm <= Sm * Sm
       and then Am * Am * Cm <= 4.0 * Jm * Jm * Jm,
     Post => Cm * T >= 0.0 and then Cm * T <= Sm and then Cm * T * (T + 0.0) >= 0.0 and then Cm * T * (T + 0.0) <= Jm
   is
      X : constant Real := Cm * T;
      Y : constant Real := Cm * T * (T + 0.0);
   begin
      Lemma_Nonnegative (Cm, T);
      Lemma_Nonnegative (X, T + 0.0);
      pragma Assert (Am = 2.0 * Y * T);
      pragma Assert (4.0 * (Y * Y * Y) = (2.0 * Y * T) * (2.0 * Y * T) * Cm);
      pragma Assert (4.0 * (Y * Y * Y) <= 4.0 * (Jm * Jm * Jm));
      Lemma_Cube_Order (Y, Jm);
      pragma Assert (X * X = Cm * Y);
      Lemma_Monotone (Cm, Cm, Y, Jm);
      pragma Assert (Cm * Jm = Jm * Cm);
      Lemma_Square_Order (X, Sm);
   end Lemma_Region_5_Peaks;

   procedure Lemma_Region_5_Is_Saturating (R : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real) is
   begin
      Lemma_Region_5_Peaks (Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max, R.T1);
   end Lemma_Region_5_Is_Saturating;

end Prunt.Motion_Planner.Trajectory.Profile_Model.Optimality;
