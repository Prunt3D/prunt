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

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile. The
--  ghost code among them computes with exact rationals and would be far too slow to run.

package body Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Retained_Side
  with SPARK_Mode => On
is
   use all type Prunt.Motion_Planner.Reals.Real;

   --  How At_Node is proved.
   --
   --  The loop of At_Node, which is the function Over_Poles, only hands predicates on, from one lemma of the
   --  package Steps, near the end of this body, to the next. Steps hides what its predicates mean, and its
   --  lemmas state that every floating-point operation which follows them is finite, so that the proof
   --  obligations of the loop itself need no arithmetic.
   --  Behind Steps, the predicates say what is known of one axis after Count poles (Is_Axis_State, and
   --  Is_Weight_State for what the doubled residues of the pairs cost), of the running product for Q
   --  (Is_Q_State), of the computed distance and ratio (Is_Setup_Accurate) and of the two weights of a pair
   --  (Is_Pair_Weight). One lemma for every group of floating-point operations moves a
   --  predicate one pole further (Lemma_Real_Axis, Lemma_Pair_Axis, Lemma_Real_Factor, Lemma_Pair_Factor) and
   --  bounds every intermediate result. Under those are the lemmas over the real numbers which carry the error
   --  analysis: every rounded operation errs by at most u = 2⁻⁵³ of its exact result plus η = 2⁻¹⁰⁷⁵ (the
   --  lemmas of Reals.Float_Lemmas), and η is absorbed into the relative error wherever the exact result is
   --  known to be far above underflow (Lemma_Normal_Rounding).
   --
   --  The budget, against the allowances of Sampled_Tangent:
   --
   --    * h errs by 3.02u relatively, the computed distance Û by at most 5u absolutely (U ≤ 1), and the ratio
   --      h/W by 4.1u.
   --    * A computed term of the retained tangent is within 9.5u of its magnitude of the exact term at Û, where
   --      its magnitude is the exact product of the stored residues and the computed weights. Count rounded
   --      additions bring the sum within (10.5 + 1.1·Count)·u of the sum M of the magnitudes, and M is at most
   --      1 + (4 + 2·Count)·u times the computed Retained_Size. Sixteen poles: 28.2u of Retained_Size.
   --    * A gathered pair holds its residues doubled by a rounded operation, so that its exact term is within
   --      1.001u of its weight, and 2⁻⁹⁷⁴, of the term of the pair in the cache; the weights are within 1 + 7.4u
   --      of M. That is another u of Retained_Size, 29.2u in all against Term_Slack = 40u, and for sixteen poles
   --      2⁻⁹⁷⁰ of underflow, which is what Retained_Dust has to cover.
   --    * Moving from Û to U changes the exact sum by at most |Û - U| ≤ 5u times Σ |r|/d², against Slope_Slack =
   --      7u. The same sum covers, with a factor 2⁻⁶⁰⁰, an underflow which a residue amplifies; every other
   --      underflow of the node loop is far below Retained_Dust.
   --    * A computed factor of Q errs relatively by at most 38u times its share of the amplification: the larger
   --      of one and the computed reciprocal of the distance from the pole, squared for a pair. The share bounds
   --      what the error of Û is amplified by, and only while the amplification is within Largest_Amplification
   --      is the error of Û small against the distance itself. The running product is then within 40u times its
   --      amplification, less the initial sixteen, of the exact partial product, relatively; relative to the
   --      computed product that is below Factor_Slack = 64u times the amplification.

   ---------------------------------------------------------------------------
   --  Real arithmetic
   ---------------------------------------------------------------------------

   procedure Lemma_Scale (C1, C2, X : Real)
   with Ghost, Pre => C1 <= C2 and then X >= 0.0, Post => C1 * X <= C2 * X;

   procedure Lemma_Scale (C1, C2, X : Real) is
   begin
      pragma Assert ((C2 - C1) * X >= 0.0);
   end Lemma_Scale;

   procedure Lemma_Cancel (X, Y, P : Real)
   with Ghost, Pre => P > 0.0 and then X * P <= Y * P, Post => X <= Y;

   procedure Lemma_Cancel (X, Y, P : Real) is
   begin
      if X > Y then
         pragma Assert ((X - Y) * P > 0.0);
         pragma Assert ((X - Y) * P = X * P - Y * P);
      end if;
   end Lemma_Cancel;

   procedure Lemma_Product_Bound (X, Y, A, B : Real)
   with Ghost, Pre => abs X <= A and then abs Y <= B, Post => abs (X * Y) <= A * B;
   --  |X| ≤ A and |Y| ≤ B give |X·Y| ≤ A·B.

   procedure Lemma_Product_Bound (X, Y, A, B : Real) is
   begin
      pragma Assert (abs (X * Y) = abs X * abs Y);
      Lemma_Scale (abs X, A, abs Y);
      Lemma_Scale (abs Y, B, A);
   end Lemma_Product_Bound;

   procedure Lemma_Product_Floor (X, Y, A, B : Real)
   with Ghost, Pre => abs X >= A and then abs Y >= B and then A >= 0.0 and then B >= 0.0, Post => abs (X * Y) >= A * B;
   --  |X| ≥ A ≥ 0 and |Y| ≥ B ≥ 0 give |X·Y| ≥ A·B.

   procedure Lemma_Product_Floor (X, Y, A, B : Real) is
   begin
      pragma Assert (abs (X * Y) = abs X * abs Y);
      Lemma_Scale (A, abs X, abs Y);
      Lemma_Scale (B, abs Y, A);
   end Lemma_Product_Floor;

   procedure Lemma_Reciprocal (X : Real)
   with
     Ghost,
     Pre  => X /= 0.0,
     Post => (1.0 / X) * X = 1.0 and then abs (1.0 / X) * abs X = 1.0 and then 1.0 / X /= 0.0;

   procedure Lemma_Reciprocal (X : Real) is
   begin
      pragma Assert ((1.0 / X) * X = 1.0);
      pragma Assert (abs ((1.0 / X) * X) = abs (1.0 / X) * abs X);
   end Lemma_Reciprocal;

   procedure Lemma_Division (X, Z : Real)
   with Ghost, Pre => Z /= 0.0, Post => X / Z = X * (1.0 / Z);

   procedure Lemma_Division (X, Z : Real) is
   begin
      pragma Assert ((X / Z) * Z = X);
      pragma Assert ((1.0 / Z) * Z = 1.0);
      pragma Assert ((X * (1.0 / Z)) * Z = X * ((1.0 / Z) * Z));
      pragma Assert ((X / Z - X * (1.0 / Z)) * Z = 0.0);
   end Lemma_Division;

   procedure Lemma_Reciprocal_Ceiling (X, Floor : Real)
   with Ghost, Pre => Floor > 0.0 and then abs X >= Floor, Post => abs (1.0 / X) * Floor <= 1.0;
   --  |X| ≥ F > 0 gives |1/X| ≤ 1/F, stated without the division.

   procedure Lemma_Reciprocal_Ceiling (X, Floor : Real) is
   begin
      Lemma_Reciprocal (X);
      Lemma_Scale (Floor, abs X, abs (1.0 / X));
   end Lemma_Reciprocal_Ceiling;

   procedure Lemma_Reciprocal_Floor (X, Ceiling : Real)
   with Ghost, Pre => X /= 0.0 and then abs X <= Ceiling, Post => abs (1.0 / X) * Ceiling >= 1.0;
   --  0 < |X| ≤ C gives |1/X| ≥ 1/C, stated without the division.

   procedure Lemma_Reciprocal_Floor (X, Ceiling : Real) is
   begin
      Lemma_Reciprocal (X);
      Lemma_Scale (abs X, Ceiling, abs (1.0 / X));
   end Lemma_Reciprocal_Floor;

   ---------------------------------------------------------------------------
   --  Relative errors
   ---------------------------------------------------------------------------

   procedure Lemma_Relative_Product (Xh, X, Yh, Y, Ex, Ey, E : Real)
   with
     Ghost,
     Pre  =>
       abs (Xh - X) <= Ex * abs X
       and then abs (Yh - Y) <= Ey * abs Y
       and then Ex >= 0.0
       and then Ey >= 0.0
       and then E >= Ex + Ey + Ex * Ey,
     Post => abs (Xh * Yh - X * Y) <= E * abs (X * Y);
   --  Relative errors of two factors add up in their product, with their product on top.

   procedure Lemma_Relative_Product (Xh, X, Yh, Y, Ex, Ey, E : Real) is
      P : constant Real := abs X * abs Y;
   begin
      pragma Assert (abs (X * Y) = P);
      pragma Assert (Xh * Yh - X * Y = (Xh - X) * Yh + X * (Yh - Y));
      pragma Assert (abs Yh <= (1.0 + Ey) * abs Y);
      Lemma_Product_Bound (Xh - X, Yh, Ex * abs X, (1.0 + Ey) * abs Y);
      pragma Assert ((Ex * abs X) * ((1.0 + Ey) * abs Y) = (Ex * (1.0 + Ey)) * P);
      Lemma_Product_Bound (X, Yh - Y, abs X, Ey * abs Y);
      pragma Assert (abs X * (Ey * abs Y) = Ey * P);
      pragma Assert (abs (Xh * Yh - X * Y) <= (Ex * (1.0 + Ey)) * P + Ey * P);
      pragma Assert ((Ex * (1.0 + Ey)) * P + Ey * P = (Ex + Ey + Ex * Ey) * P);
      Lemma_Scale (Ex + Ey + Ex * Ey, E, P);
   end Lemma_Relative_Product;

   procedure Lemma_Relative_Chain (Zh, Z, T, E1, E2, E : Real)
   with
     Ghost,
     Pre  =>
       abs (Zh - Z) <= E1 * abs Z
       and then abs (Z - T) <= E2 * abs T
       and then E1 >= 0.0
       and then E2 >= 0.0
       and then E >= E1 + E2 + E1 * E2,
     Post => abs (Zh - T) <= E * abs T;
   --  Zh approximates Z, which approximates T.

   procedure Lemma_Relative_Chain (Zh, Z, T, E1, E2, E : Real) is
   begin
      pragma Assert (abs Z <= (1.0 + E2) * abs T);
      Lemma_Scale (abs Z, (1.0 + E2) * abs T, E1);
      pragma Assert (E1 * abs Z <= E1 * ((1.0 + E2) * abs T));
      pragma Assert (E1 * ((1.0 + E2) * abs T) = (E1 + E1 * E2) * abs T);
      pragma Assert (abs (Zh - T) <= (E1 + E1 * E2) * abs T + E2 * abs T);
      pragma Assert ((E1 + E1 * E2) * abs T + E2 * abs T = (E1 + E2 + E1 * E2) * abs T);
      Lemma_Scale (E1 + E2 + E1 * E2, E, abs T);
   end Lemma_Relative_Chain;

   procedure Lemma_Relative_Reciprocal (Xh, X, Ex, E : Real)
   with
     Ghost,
     Pre  =>
       X /= 0.0 and then abs (Xh - X) <= Ex * abs X and then Ex >= 0.0 and then Ex < 1.0 and then E * (1.0 - Ex) >= Ex,
     Post => Xh /= 0.0 and then abs (1.0 / Xh - 1.0 / X) <= E * abs (1.0 / X);
   --  The reciprocal of an approximation.

   procedure Lemma_Relative_Reciprocal (Xh, X, Ex, E : Real) is
      Dv : constant Real := 1.0 / Xh - 1.0 / X;
   begin
      pragma Assert (abs Xh >= (1.0 - Ex) * abs X);
      pragma Assert ((1.0 - Ex) * abs X > 0.0);
      pragma Assert (Xh /= 0.0);
      Lemma_Reciprocal (X);
      Lemma_Reciprocal (Xh);
      pragma Assert (Dv * Xh = 1.0 - (1.0 / X) * Xh);
      pragma Assert ((Dv * Xh) * X = X - Xh);
      pragma Assert (abs ((Dv * Xh) * X) = (abs Dv * abs Xh) * abs X);
      pragma Assert ((abs Dv * abs Xh) * abs X <= Ex * abs X);
      pragma Assert (abs Dv * abs Xh <= Ex);
      Lemma_Scale ((1.0 - Ex) * abs X, abs Xh, abs Dv);
      pragma Assert (abs Dv * ((1.0 - Ex) * abs X) <= Ex);
      pragma Assert ((abs Dv * abs X) * (1.0 - Ex) <= E * (1.0 - Ex));
      pragma Assert (abs Dv * abs X <= E);
      Lemma_Scale (abs Dv * abs X, E, abs (1.0 / X));
      pragma Assert ((abs Dv * abs X) * abs (1.0 / X) = abs Dv * (abs (1.0 / X) * abs X));
   end Lemma_Relative_Reciprocal;

   Normal_Floor : constant := 2.0**(-1000);
   --  An operation whose exact result is at least this large in magnitude

   Unit : constant := 1.001 * 2.0**(-53);
   --  errs by at most this relative to it: the underflow term is absorbed.

   procedure Lemma_Normal_Rounding (Zh, Z : Real)
   with
     Ghost,
     Pre  => abs (Zh - Z) <= Epsilon * abs Z + Eta and then abs Z >= Normal_Floor,
     Post => abs (Zh - Z) <= Unit * abs Z;

   procedure Lemma_Normal_Rounding (Zh, Z : Real) is
      pragma Unreferenced (Zh);
   begin
      pragma Assert (Eta <= 0.001 * Epsilon * Normal_Floor);
      pragma Assert (Eta <= 0.001 * Epsilon * abs Z);
   end Lemma_Normal_Rounding;

   ---------------------------------------------------------------------------
   --  The warp scale, the distance and the ratio
   ---------------------------------------------------------------------------

   Smallest_Scale : constant := 2.0**(-24);
   Largest_Scale  : constant := 2.0**24;
   --  Largest_Warp and Smallest_Warp, as real numbers: h lies between one and the warp.

   Margin : constant := 2.0**(-20);
   --  Node_Margin, as a real number.

   Smallest_Product : constant := 2.0**(-44);
   --  Smallest_Scale times Margin.

   Smallest_Ratio : constant := 2.0**(-48);
   --  Smallest_Scale divided by Largest_Scale.

   Complement_Error : constant := 2.0**(-53) + 2.0**(-1055);
   --  Sample_Nodes.Complement_Relative_Error: the relative error of the stored complement of an accurate sample.

   Scale_Error : constant := 3.02 * 2.0**(-53);
   --  The relative error of the computed h: a rounded complement, a product and a sum of nonnegative terms.

   Distance_Error : constant := 5.0 * 2.0**(-53);
   --  The computed distance is within this of U, which is at most one.

   Ratio_Error : constant := 4.1 * 2.0**(-53);
   --  The relative error of the computed h/W.

   procedure Lemma_Setup_Ranges (Warp, Node, Complement : Dimensionless)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Node >= 0.0
       and then Node <= 1.0
       and then Complement >= 0.0
       and then Complement <= 1.0
       and then Node + Complement >= 0.5,
     Post =>
       Warp * Complement + Node >= 2.0**(-28)
       and then Warp * Complement + Node <= 2.0**25
       and then Node / (Warp * Complement + Node) >= 0.0
       and then Node / (Warp * Complement + Node) <= 2.0**28
       and then Dimensionless'Min (1.0, Node / (Warp * Complement + Node)) >= 0.0
       and then Dimensionless'Min (1.0, Node / (Warp * Complement + Node)) <= 1.0
       and then (Warp * Complement + Node) / Warp >= 2.0**(-53)
       and then (Warp * Complement + Node) / Warp <= 2.0**50;
   --  The magnitudes of h, of the distance and of h/W, whatever the node.

   procedure Lemma_Ratio_Range (H, Warp : Dimensionless)
   with
     Ghost,
     Pre  => H >= 2.0**(-28) and then H <= 2.0**25 and then Warp >= Smallest_Warp and then Warp <= Largest_Warp,
     Post => H / Warp >= 2.0**(-53) and then H / Warp <= 2.0**50;

   procedure Lemma_Ratio_Range (H, Warp : Dimensionless) is null;

   procedure Lemma_Half_Sum (Node, Complement : Dimensionless)
   with
     Ghost,
     Pre  =>
       Node >= 0.0
       and then Node <= 1.0
       and then Complement >= 0.0
       and then Complement <= 1.0
       and then Node + Complement >= 0.5,
     Post => Node >= 0.125 or else Complement >= 0.125;
   --  Two numbers with a sum of at least a half are not both small.

   procedure Lemma_Half_Sum (Node, Complement : Dimensionless) is null;

   procedure Lemma_Warp_Product (Warp, Complement : Dimensionless)
   with
     Ghost,
     Pre  => Warp >= Smallest_Warp and then Warp <= Largest_Warp and then Complement >= 0.0 and then Complement <= 1.0,
     Post =>
       Warp * Complement >= 0.0
       and then Warp * Complement <= 2.0**24
       and then (if Complement >= 0.125 then Warp * Complement >= 2.0**(-27));

   procedure Lemma_Warp_Product (Warp, Complement : Dimensionless) is null;

   procedure Lemma_Scale_Sum (Product, Node : Dimensionless)
   with
     Ghost,
     Pre  =>
       Product >= 0.0
       and then Product <= 2.0**24
       and then Node >= 0.0
       and then Node <= 1.0
       and then (Product >= 2.0**(-27) or else Node >= 0.125),
     Post => Product + Node >= 2.0**(-28) and then Product + Node <= 2.0**25;

   procedure Lemma_Scale_Sum (Product, Node : Dimensionless) is null;

   procedure Lemma_Distance_Range (Node, H : Dimensionless)
   with
     Ghost,
     Pre  => Node >= 0.0 and then Node <= 1.0 and then H >= 2.0**(-28) and then H <= 2.0**25,
     Post =>
       Node / H >= 0.0
       and then Node / H <= 2.0**28
       and then Dimensionless'Min (1.0, Node / H) >= 0.0
       and then Dimensionless'Min (1.0, Node / H) <= 1.0;

   procedure Lemma_Distance_Range (Node, H : Dimensionless) is null;

   procedure Lemma_Setup_Ranges (Warp, Node, Complement : Dimensionless) is
   begin
      Lemma_Half_Sum (Node, Complement);
      Lemma_Warp_Product (Warp, Complement);
      Lemma_Scale_Sum (Warp * Complement, Node);
      Lemma_Distance_Range (Node, Warp * Complement + Node);
      Lemma_Ratio_Range (Warp * Complement + Node, Warp);
   end Lemma_Setup_Ranges;

   procedure Lemma_Minimum (T : Dimensionless)
   with
     Ghost,
     Pre  => T >= 0.0 and then T <= 2.0**28,
     Post => (if R (T) <= 1.0 then R (Dimensionless'Min (1.0, T)) = R (T) else R (Dimensionless'Min (1.0, T)) = 1.0);

   procedure Lemma_Minimum (T : Dimensionless) is null;

   procedure Lemma_Scale_Is_Bounded (W, X : Real)
   with
     Ghost,
     Pre  => W >= Smallest_Scale and then W <= Largest_Scale and then X >= 0.0 and then X <= 1.0,
     Post =>
       W * (1.0 - X) + X >= Smallest_Scale
       and then W * (1.0 - X) + X <= Largest_Scale
       and then W * (1.0 - X) + X >= X
       and then W * (1.0 - X) >= 0.0
       and then W * (1.0 - X) >= Smallest_Scale * (1.0 - X);
   --  h = W·(1 - X) + X lies between W and one, and is at least X.

   procedure Lemma_Scale_Is_Bounded (W, X : Real) is
   begin
      Lemma_Scale (Smallest_Scale, W, 1.0 - X);
      Lemma_Scale (W, Largest_Scale, 1.0 - X);
   end Lemma_Scale_Is_Bounded;

   procedure Lemma_Scale_Error (W, X, C, P, H : Real)
   with
     Ghost,
     Pre  =>
       W >= Smallest_Scale
       and then W <= Largest_Scale
       and then X >= Margin
       and then X <= 1.0 - Margin
       and then abs (C - (1.0 - X)) <= Complement_Error * (1.0 - X)
       and then abs (P - W * C) <= Epsilon * abs (W * C) + Eta
       and then abs (H - (P + X)) <= Epsilon * abs (P + X) + Eta,
     Post => abs (H - (W * (1.0 - X) + X)) <= Scale_Error * (W * (1.0 - X) + X);
   --  P is the rounded product W·C and H the rounded sum P + X.

   procedure Lemma_Scale_Error (W, X, C, P, H : Real) is
      Exact_Product : constant Real := W * (1.0 - X);
      Scale         : constant Real := W * (1.0 - X) + X;
   begin
      Lemma_Scale_Is_Bounded (W, X);
      pragma Assert (Exact_Product >= Smallest_Scale * Margin);
      pragma Assert (Exact_Product >= Smallest_Product);
      --  The product.
      Lemma_Relative_Product (W, W, C, 1.0 - X, 0.0, Complement_Error, Complement_Error);
      pragma Assert (abs (W * C - Exact_Product) <= Complement_Error * abs Exact_Product);
      pragma Assert (abs (W * C) >= (1.0 - Complement_Error) * Exact_Product);
      pragma Assert (abs (W * C) >= Normal_Floor);
      Lemma_Normal_Rounding (P, W * C);
      Lemma_Relative_Chain (P, W * C, Exact_Product, Unit, Complement_Error, 2.01 * Epsilon);
      pragma Assert (abs (P - Exact_Product) <= 2.01 * Epsilon * Exact_Product);
      --  The sum.
      pragma Assert (abs ((P + X) - Scale) <= 2.01 * Epsilon * Exact_Product);
      pragma Assert (Exact_Product <= Scale);
      pragma Assert (2.01 * Epsilon * Exact_Product <= 2.01 * Epsilon * Scale);
      pragma Assert (abs ((P + X) - Scale) <= 2.01 * Epsilon * abs Scale);
      pragma Assert (abs (P + X) >= (1.0 - 2.01 * Epsilon) * Scale);
      pragma Assert (abs (P + X) >= Normal_Floor);
      Lemma_Normal_Rounding (H, P + X);
      Lemma_Relative_Chain (H, P + X, Scale, Unit, 2.01 * Epsilon, Scale_Error);
   end Lemma_Scale_Error;

   procedure Lemma_Distance_Error (X, Scale, H, T : Real)
   with
     Ghost,
     Pre  =>
       X >= Margin
       and then X <= 1.0
       and then Scale >= Smallest_Scale
       and then Scale <= Largest_Scale
       and then Scale >= X
       and then abs (H - Scale) <= Scale_Error * Scale
       and then H /= 0.0
       and then abs (T - X / H) <= Epsilon * abs (X / H) + Eta,
     Post => X / Scale >= 0.0 and then X / Scale <= 1.0 and then abs (T - X / Scale) <= 4.1 * Epsilon;
   --  T is the rounded quotient of X by the computed scale H.

   procedure Lemma_Distance_Error (X, Scale, H, T : Real) is
      Distance : constant Real := X / Scale;
   begin
      Lemma_Reciprocal (Scale);
      pragma Assert (Distance = X * (1.0 / Scale));
      pragma Assert (Distance * Scale = X);
      pragma Assert (1.0 / Scale > 0.0);
      pragma Assert (Distance >= 0.0);
      pragma Assert (Distance <= 1.0);
      Lemma_Reciprocal_Floor (Scale, Largest_Scale);
      pragma Assert (1.0 / Scale >= Smallest_Scale);
      Lemma_Scale (Smallest_Scale, 1.0 / Scale, X);
      pragma Assert (Distance >= Smallest_Scale * Margin);
      pragma Assert (Distance >= Smallest_Product);
      Lemma_Relative_Reciprocal (H, Scale, Scale_Error, 3.03 * Epsilon);
      Lemma_Relative_Product (X, X, 1.0 / H, 1.0 / Scale, 0.0, 3.03 * Epsilon, 3.03 * Epsilon);
      pragma Assert (X / H = X * (1.0 / H));
      pragma Assert (abs (X / H - Distance) <= 3.03 * Epsilon * abs Distance);
      pragma Assert (abs (X / H) >= (1.0 - 3.03 * Epsilon) * Distance);
      pragma Assert (abs (X / H) >= Normal_Floor);
      Lemma_Normal_Rounding (T, X / H);
      Lemma_Relative_Chain (T, X / H, Distance, Unit, 3.03 * Epsilon, 4.1 * Epsilon);
      pragma Assert (abs (T - Distance) <= 4.1 * Epsilon * abs Distance);
   end Lemma_Distance_Error;

   procedure Lemma_Ratio_Error (W, Scale, H, Q : Real)
   with
     Ghost,
     Pre  =>
       W >= Smallest_Scale
       and then W <= Largest_Scale
       and then Scale >= Smallest_Scale
       and then Scale <= Largest_Scale
       and then abs (H - Scale) <= Scale_Error * Scale
       and then abs (Q - H / W) <= Epsilon * abs (H / W) + Eta,
     Post => Scale / W >= Smallest_Ratio and then abs (Q - Scale / W) <= Ratio_Error * (Scale / W);
   --  Q is the rounded quotient of the computed scale H by the warp.

   procedure Lemma_Ratio_Error (W, Scale, H, Q : Real) is
      Ratio : constant Real := Scale / W;
   begin
      Lemma_Reciprocal (W);
      Lemma_Reciprocal_Floor (W, Largest_Scale);
      pragma Assert (1.0 / W >= Smallest_Scale);
      pragma Assert (Ratio = Scale * (1.0 / W));
      Lemma_Scale (Smallest_Scale, Scale, 1.0 / W);
      pragma Assert (Ratio >= Smallest_Scale * (1.0 / W));
      pragma Assert (Ratio >= Smallest_Scale * Smallest_Scale);
      pragma Assert (Ratio >= Smallest_Ratio);
      pragma Assert (H / W = H * (1.0 / W));
      Lemma_Relative_Product (H, Scale, 1.0 / W, 1.0 / W, Scale_Error, 0.0, Scale_Error);
      pragma Assert (abs (H / W - Ratio) <= Scale_Error * abs Ratio);
      pragma Assert (abs (H / W) >= (1.0 - Scale_Error) * Ratio);
      pragma Assert (abs (H / W) >= Normal_Floor);
      Lemma_Normal_Rounding (Q, H / W);
      Lemma_Relative_Chain (Q, H / W, Ratio, Unit, Scale_Error, Ratio_Error);
   end Lemma_Ratio_Error;

   function Is_Setup_Accurate (Warp : Dimensionless; V : Real; U, Ratio : Dimensionless) return Boolean
   with
     Ghost,
     Pre  => Warp >= Smallest_Warp and then Warp <= Largest_Warp,
     Post =>
       Is_Setup_Accurate'Result
       = (V >= 0.0
          and then V <= 1.0
          and then Model.Warp_Scale (Warp, V) >= Smallest_Scale
          and then Model.Warp_Scale (Warp, V) <= Largest_Scale
          and then Model.Distance (Warp, V) = V / Model.Warp_Scale (Warp, V)
          and then Model.Distance (Warp, V) >= 0.0
          and then Model.Distance (Warp, V) <= 1.0
          and then abs (R (U) - Model.Distance (Warp, V)) <= Distance_Error
          and then Model.Warp_Scale (Warp, V) / R (Warp) >= Smallest_Ratio
          and then
            abs (R (Ratio) - Model.Warp_Scale (Warp, V) / R (Warp))
            <= Ratio_Error * (Model.Warp_Scale (Warp, V) / R (Warp)));
   --  The computed distance U and ratio h/W are those of chart coordinate V, to within their rounding. This and
   --  the other predicates of the node loop are ordinary functions with a postcondition, not expression
   --  functions, to have the tool keep each as one term where it is only handed on.

   function Is_Setup_Accurate (Warp : Dimensionless; V : Real; U, Ratio : Dimensionless) return Boolean is
   begin
      return
        V >= 0.0
        and then V <= 1.0
        and then Model.Warp_Scale (Warp, V) >= Smallest_Scale
        and then Model.Warp_Scale (Warp, V) <= Largest_Scale
        and then Model.Distance (Warp, V) = V / Model.Warp_Scale (Warp, V)
        and then Model.Distance (Warp, V) >= 0.0
        and then Model.Distance (Warp, V) <= 1.0
        and then abs (R (U) - Model.Distance (Warp, V)) <= Distance_Error
        and then Model.Warp_Scale (Warp, V) / R (Warp) >= Smallest_Ratio
        and then
          abs (R (Ratio) - Model.Warp_Scale (Warp, V) / R (Warp))
          <= Ratio_Error * (Model.Warp_Scale (Warp, V) / R (Warp));
   end Is_Setup_Accurate;

   procedure Lemma_Setup_Intro (Warp : Dimensionless; V : Real; U, Ratio : Dimensionless)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then V >= 0.0
       and then V <= 1.0
       and then Model.Warp_Scale (Warp, V) >= Smallest_Scale
       and then Model.Warp_Scale (Warp, V) <= Largest_Scale
       and then Model.Distance (Warp, V) = V / Model.Warp_Scale (Warp, V)
       and then Model.Distance (Warp, V) >= 0.0
       and then Model.Distance (Warp, V) <= 1.0
       and then abs (R (U) - Model.Distance (Warp, V)) <= Distance_Error
       and then Model.Warp_Scale (Warp, V) / R (Warp) >= Smallest_Ratio
       and then
         abs (R (Ratio) - Model.Warp_Scale (Warp, V) / R (Warp))
         <= Ratio_Error * (Model.Warp_Scale (Warp, V) / R (Warp)),
     Post => Is_Setup_Accurate (Warp, V, U, Ratio);

   procedure Lemma_Setup_Intro (Warp : Dimensionless; V : Real; U, Ratio : Dimensionless) is null;

   procedure Lemma_Setup_Elim (Warp : Dimensionless; V : Real; U, Ratio : Dimensionless)
   with
     Ghost,
     Pre  => Warp >= Smallest_Warp and then Warp <= Largest_Warp and then Is_Setup_Accurate (Warp, V, U, Ratio),
     Post =>
       V >= 0.0
       and then V <= 1.0
       and then Model.Warp_Scale (Warp, V) >= Smallest_Scale
       and then Model.Warp_Scale (Warp, V) <= Largest_Scale
       and then Model.Distance (Warp, V) = V / Model.Warp_Scale (Warp, V)
       and then Model.Distance (Warp, V) >= 0.0
       and then Model.Distance (Warp, V) <= 1.0
       and then abs (R (U) - Model.Distance (Warp, V)) <= Distance_Error
       and then Model.Warp_Scale (Warp, V) / R (Warp) >= Smallest_Ratio
       and then
         abs (R (Ratio) - Model.Warp_Scale (Warp, V) / R (Warp))
         <= Ratio_Error * (Model.Warp_Scale (Warp, V) / R (Warp));

   procedure Lemma_Setup_Elim (Warp : Dimensionless; V : Real; U, Ratio : Dimensionless) is null;

   procedure Lemma_Setup_Accuracy (Warp, Node, Complement : Dimensionless)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Node >= Sample_Nodes.Node_Margin
       and then Node <= 1.0 - Sample_Nodes.Node_Margin
       and then Complement >= 0.0
       and then Complement <= 1.0
       and then Node + Complement >= 0.5
       and then abs (R (Complement) - (1.0 - R (Node))) <= Complement_Error * (1.0 - R (Node)),
     Post =>
       Is_Setup_Accurate
         (Warp,
          R (Node),
          Dimensionless'Min (1.0, Node / (Warp * Complement + Node)),
          (Warp * Complement + Node) / Warp);
   --  What At_Node computes first, at an accurate sample.

   procedure Lemma_Setup_Accuracy (Warp, Node, Complement : Dimensionless) is
      W : constant Real := R (Warp);
      X : constant Real := R (Node);
   begin
      Lemma_Setup_Ranges (Warp, Node, Complement);
      pragma Assert (W >= Smallest_Scale and then W <= Largest_Scale);
      pragma Assert (X >= Margin and then X <= 1.0 - Margin);
      Lemma_Scale_Is_Bounded (W, X);
      declare
         H        : constant Dimensionless := Warp * Complement + Node;
         T        : constant Dimensionless := Node / H;
         Scale    : constant Real := W * (1.0 - X) + X;
         Distance : constant Real := X / Scale;
      begin
         pragma Assert (Model.Warp_Scale (Warp, X) = Scale);
         pragma Assert (Model.Distance (Warp, X) = Distance);
         Float_Lemmas.Lemma_Rounding_Error_Mul (Warp, Complement);
         Float_Lemmas.Lemma_Rounding_Error_Add (Warp * Complement, Node);
         Lemma_Scale_Error (W, X, R (Complement), R (Warp * Complement), R (H));
         pragma Assert (R (H) /= 0.0);
         Float_Lemmas.Lemma_Rounding_Error_Div (Node, H);
         Lemma_Distance_Error (X, Scale, R (H), R (T));
         Lemma_Minimum (T);
         pragma Assert (abs (R (Dimensionless'Min (1.0, T)) - Distance) <= abs (R (T) - Distance));
         Float_Lemmas.Lemma_Rounding_Error_Div (H, Warp);
         Lemma_Ratio_Error (W, Scale, R (H), R (H / Warp));
         pragma Assert (abs (R (Dimensionless'Min (1.0, T)) - Model.Distance (Warp, X)) <= Distance_Error);
         Lemma_Setup_Intro (Warp, X, Dimensionless'Min (1.0, T), H / Warp);
      end;
   end Lemma_Setup_Accuracy;

   ---------------------------------------------------------------------------
   --  The gathered poles and the cache
   ---------------------------------------------------------------------------

   function Is_Term_Ready (Term : Pole_Term) return Boolean
   is (Poles.Is_Term_Bounded (Term)
       and then (if Term.Imaginary_Part = 0.0 then Term.Real_Part < 0.0 or else Term.Real_Part > 1.0))
   with Ghost;
   --  What the node loop needs of one gathered pole, whichever pole of the cache it is.

   procedure Lemma_Term_Kind (Cache : Rational_Antiderivative; Prepared : Preparation; Index : Rational_Degree_Slot)
   with
     Ghost,
     Pre  => Has_Layout (Cache) and then Poles.Is_Cache_Prepared (Cache, Prepared) and then Index <= Prepared.Count,
     Post =>
       Poles.Is_Term_Bounded (Prepared.Terms (Index))
       and then Is_Term_Ready (Prepared.Terms (Index))
       and then abs Prepared.Terms (Index).Real_Part <= Magnitude_Limit
       and then abs Prepared.Terms (Index).Reciprocal_Norm <= Magnitude_Limit
       and then Prepared.Terms (Index).Imaginary_Part >= 0.0
       and then Prepared.Terms (Index).Imaginary_Part <= Magnitude_Limit
       and then Prepared.Count = Cache.Real_Pole_Count + Cache.Pair_Count
       and then (Prepared.Terms (Index).Imaginary_Part = 0.0) = (Index <= Cache.Real_Pole_Count)
       and then
         (if Index <= Cache.Real_Pole_Count
          then Poles.Is_Real_Term (Cache, Index, Prepared.Terms (Index))
          else
            Index - Cache.Real_Pole_Count <= Cache.Pair_Count
            and then Poles.Is_Pair_Term (Cache, Index - Cache.Real_Pole_Count, Prepared.Terms (Index)));
   --  The first terms are the real poles of the cache, the others its pairs, and the node loop tells them
   --  apart by the imaginary part.

   procedure Lemma_Term_Kind (Cache : Rational_Antiderivative; Prepared : Preparation; Index : Rational_Degree_Slot) is
   begin
      pragma Assert (Poles.Is_Term_Bounded (Prepared.Terms (Index)));
      if Index <= Cache.Real_Pole_Count then
         pragma Assert (Poles.Is_Real_Term (Cache, Index, Prepared.Terms (Index)));
      else
         declare
            Pair : constant Rational_Pair_Index := Index - Cache.Real_Pole_Count;
         begin
            pragma Assert (Pair <= Cache.Pair_Count);
            declare
               Found : constant Boolean :=
                 Poles.Is_Pair_Term (Cache, Pair, Prepared.Terms (Cache.Real_Pole_Count + Pair));
               --  A constant, for the tool to take the fact whole from Is_Cache_Prepared: asserted as a call, it
               --  would be proved anew one part at a time.
            begin
               pragma Assert (Found);
               pragma Assert (Prepared.Terms (Index).Imaginary_Part > 0.0);
            end;
         end;
      end if;
   end Lemma_Term_Kind;

   function Term_Value (Term : Pole_Term; Axis : Axis_Name; V : Real) return Real
   is (if Term.Imaginary_Part = 0.0
       then (if V = R (Term.Real_Part) then 0.0 else R (Term.Residue_Real (Axis)) / (V - R (Term.Real_Part)))
       elsif (V - R (Term.Real_Part)) * (V - R (Term.Real_Part)) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)
         = 0.0
       then 0.0
       else
         (R (Term.Residue_Real (Axis)) * (V - R (Term.Real_Part))
          - R (Term.Residue_Imaginary (Axis)) * R (Term.Imaginary_Part))
         / ((V - R (Term.Real_Part)) * (V - R (Term.Real_Part)) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
   with Ghost;
   --  The term of one gathered pole in the retained tangent at V: r/(V - p), or for a pair, whose residue is
   --  stored doubled, (R·(V - A) - I·B)/((V - A)² + B²).

   function Term_Sum (Prepared : Preparation; Count : Natural; Axis : Axis_Name; V : Real) return Real
   is (if Count = 0
       then 0.0
       else Term_Sum (Prepared, Count - 1, Axis, V) + Term_Value (Prepared.Terms (Count), Axis, V))
   with Ghost, Pre => Count <= Maximum_Rational_Degree, Subprogram_Variant => (Decreases => Count);
   --  The terms of the first Count gathered poles.

   function Real_Count (Cache : Rational_Antiderivative; Count : Natural) return Natural
   is (if Count <= Cache.Real_Pole_Count then Count else Cache.Real_Pole_Count)
   with Ghost;
   --  How many of the first Count gathered poles are real,

   function Pair_Count (Cache : Rational_Antiderivative; Count : Natural) return Natural
   is (if Count <= Cache.Real_Pole_Count then 0 else Count - Cache.Real_Pole_Count)
   with Ghost;
   --  and how many are pairs.

   ---------------------------------------------------------------------------
   --  The cached denominator, one factor at a time
   ---------------------------------------------------------------------------

   function Partial_Denominator
     (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless; Count : Natural; V : Real) return Real
   is (R (Denominator_Scale)
       * Model.Real_Product (Cache, Real_Count (Cache, Count), Warp, V)
       * Model.Pair_Product (Cache, Pair_Count (Cache, Count), Warp, V))
   with Ghost, Pre => Has_Layout (Cache) and then Count <= Cache.Real_Pole_Count + Cache.Pair_Count;
   --  Q with only the factors of the first Count gathered poles.

   procedure Lemma_Partial_Start (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless; V : Real)
   with
     Ghost,
     Pre  => Has_Layout (Cache),
     Post =>
       Partial_Denominator (Cache, Denominator_Scale, Warp, 0, V) = R (Denominator_Scale)
       and then
         Partial_Denominator (Cache, Denominator_Scale, Warp, Cache.Real_Pole_Count + Cache.Pair_Count, V)
         = Model.Cached_Denominator (Cache, Denominator_Scale, Warp, V);

   procedure Lemma_Partial_Start (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless; V : Real)
   is
      pragma Unreferenced (Denominator_Scale);
   begin
      pragma Assert (Model.Real_Product (Cache, 0, Warp, V) = 1.0);
      pragma Assert (Model.Pair_Product (Cache, 0, Warp, V) = 1.0);
      pragma Assert (Real_Count (Cache, Cache.Real_Pole_Count + Cache.Pair_Count) = Cache.Real_Pole_Count);
      pragma Assert (Pair_Count (Cache, Cache.Real_Pole_Count + Cache.Pair_Count) = Cache.Pair_Count);
   end Lemma_Partial_Start;

   procedure Lemma_Partial_Real_Step
     (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless; Count : Positive; V : Real)
   with
     Ghost,
     Pre  => Has_Layout (Cache) and then Count <= Cache.Real_Pole_Count,
     Post =>
       Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V)
       = Partial_Denominator (Cache, Denominator_Scale, Warp, Count - 1, V)
         * Model.Real_Factor (Cache, Count, Warp, V);

   procedure Lemma_Partial_Real_Step
     (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless; Count : Positive; V : Real)
   is
      S : constant Real := R (Denominator_Scale);
      P : constant Real := Model.Real_Product (Cache, Count - 1, Warp, V);
      F : constant Real := Model.Real_Factor (Cache, Count, Warp, V);
   begin
      pragma Assert (Model.Real_Product (Cache, Count, Warp, V) = P * F);
      pragma Assert (Model.Pair_Product (Cache, 0, Warp, V) = 1.0);
      pragma Assert (Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V) = S * (P * F) * 1.0);
      pragma Assert (Partial_Denominator (Cache, Denominator_Scale, Warp, Count - 1, V) = S * P * 1.0);
      pragma Assert (S * (P * F) * 1.0 = (S * P * 1.0) * F);
   end Lemma_Partial_Real_Step;

   procedure Lemma_Partial_Pair_Step
     (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless; Count : Positive; V : Real)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Count > Cache.Real_Pole_Count
       and then Count <= Cache.Real_Pole_Count + Cache.Pair_Count,
     Post =>
       Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V)
       = Partial_Denominator (Cache, Denominator_Scale, Warp, Count - 1, V)
         * Model.Pair_Factor (Cache, Count - Cache.Real_Pole_Count, Warp, V);

   procedure Lemma_Partial_Pair_Step
     (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless; Count : Positive; V : Real)
   is
      Pair : constant Rational_Pair_Index := Count - Cache.Real_Pole_Count;
      S    : constant Real := R (Denominator_Scale);
      A    : constant Real := Model.Real_Product (Cache, Cache.Real_Pole_Count, Warp, V);
      P    : constant Real := Model.Pair_Product (Cache, Pair - 1, Warp, V);
      F    : constant Real := Model.Pair_Factor (Cache, Pair, Warp, V);
   begin
      pragma Assert (Model.Pair_Product (Cache, Pair, Warp, V) = P * F);
      pragma Assert (Real_Count (Cache, Count) = Cache.Real_Pole_Count);
      pragma Assert (Real_Count (Cache, Count - 1) = Cache.Real_Pole_Count);
      pragma Assert (Pair_Count (Cache, Count) = Pair and then Pair_Count (Cache, Count - 1) = Pair - 1);
      pragma Assert (Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V) = S * A * (P * F));
      pragma Assert (Partial_Denominator (Cache, Denominator_Scale, Warp, Count - 1, V) = S * A * P);
      pragma Assert (S * A * (P * F) = (S * A * P) * F);
   end Lemma_Partial_Pair_Step;

   procedure Lemma_Real_Factor_Form (P, W, H, V : Real)
   with
     Ghost,
     Pre  => P /= 0.0 and then W > 0.0 and then H > 0.0,
     Post => P * W /= 0.0 and then (P * H - V) / (P * W) = (H / W) * ((P - V / H) / P);
   --  (V - p·h)/(-p·W) = (h/W)·(p - U)/p with U = V/h.

   procedure Lemma_Real_Factor_Form (P, W, H, V : Real) is
      Left  : constant Real := (P * H - V) / (P * W);
      Right : constant Real := (H / W) * ((P - V / H) / P);
   begin
      pragma Assert (P * W /= 0.0);
      pragma Assert (Left * (P * W) = P * H - V);
      pragma Assert ((H / W) * W = H);
      pragma Assert (((P - V / H) / P) * P = P - V / H);
      pragma Assert ((V / H) * H = V);
      pragma Assert (H * (P - V / H) = P * H - V);
      pragma Assert (Right * (P * W) = ((H / W) * W) * (((P - V / H) / P) * P));
      pragma Assert (Right * (P * W) = P * H - V);
      pragma Assert ((Left - Right) * (P * W) = 0.0);
   end Lemma_Real_Factor_Form;

   procedure Lemma_Pair_Factor_Form (A, B, W, H, V : Real)
   with
     Ghost,
     Pre  => A * A + B * B > 0.0 and then W > 0.0 and then H > 0.0,
     Post =>
       (A * A + B * B) * (W * W) /= 0.0
       and then
         ((V - A * H) * (V - A * H) + (B * H) * (B * H)) / ((A * A + B * B) * (W * W))
         = ((H / W) * (H / W)) * (((V / H - A) * (V / H - A) + B * B) / (A * A + B * B));
   --  |V - π·h|²/(|π|²·W²) = (h/W)²·|U - π|²/|π|² with U = V/h.

   procedure Lemma_Pair_Factor_Form (A, B, W, H, V : Real) is
      N     : constant Real := A * A + B * B;
      U     : constant Real := V / H;
      Left  : constant Real := ((V - A * H) * (V - A * H) + (B * H) * (B * H)) / (N * (W * W));
      Inner : constant Real := ((U - A) * (U - A) + B * B) / N;
      Right : constant Real := ((H / W) * (H / W)) * Inner;
   begin
      pragma Assert (W * W > 0.0);
      pragma Assert (N * (W * W) /= 0.0);
      pragma Assert (Left * (N * (W * W)) = (V - A * H) * (V - A * H) + (B * H) * (B * H));
      pragma Assert (U * H = V);
      pragma Assert (V - A * H = (U - A) * H);
      pragma Assert ((V - A * H) * (V - A * H) = ((U - A) * (U - A)) * (H * H));
      pragma Assert ((B * H) * (B * H) = (B * B) * (H * H));
      pragma Assert (Left * (N * (W * W)) = ((U - A) * (U - A) + B * B) * (H * H));
      pragma Assert (Inner * N = (U - A) * (U - A) + B * B);
      pragma Assert ((H / W) * W = H);
      pragma Assert (((H / W) * (H / W)) * (W * W) = ((H / W) * W) * ((H / W) * W));
      pragma Assert (((H / W) * (H / W)) * (W * W) = H * H);
      pragma Assert (Right * (N * (W * W)) = (((H / W) * (H / W)) * (W * W)) * (Inner * N));
      pragma Assert (Right * (N * (W * W)) = (H * H) * ((U - A) * (U - A) + B * B));
      pragma Assert ((Left - Right) * (N * (W * W)) = 0.0);
   end Lemma_Pair_Factor_Form;

   ---------------------------------------------------------------------------
   --  Moving the retained tangent from the computed distance to the exact one
   ---------------------------------------------------------------------------

   Smallest_Square : constant := 2.0**(-200);
   --  Poles.Smallest_Distance_Square.

   Reach : constant := 2.0**(-100);
   --  Its square root: no real pole of a prepared cache is closer to [0, 1].

   procedure Lemma_Square_Order (X, Y : Real)
   with Ghost, Pre => X >= 0.0 and then Y >= 0.0 and then X * X <= Y * Y, Post => X <= Y;

   procedure Lemma_Square_Order (X, Y : Real) is
   begin
      if X > Y then
         pragma Assert (X + Y > 0.0);
         pragma Assert ((X - Y) * (X + Y) > 0.0);
         pragma Assert ((X - Y) * (X + Y) = X * X - Y * Y);
      end if;
   end Lemma_Square_Order;

   procedure Lemma_Quotient_Bound (X, Numerator, Divisor, Step : Real)
   with
     Ghost,
     Pre  => Divisor > 0.0 and then X * Divisor <= Numerator * Step,
     Post => X <= Step * (Numerator / Divisor);
   --  X·D ≤ N·S with D > 0 gives X ≤ S·(N/D).

   procedure Lemma_Quotient_Bound (X, Numerator, Divisor, Step : Real) is
      Q : constant Real := Numerator / Divisor;
   begin
      pragma Assert (Q * Divisor = Numerator);
      pragma Assert ((Step * Q) * Divisor = Numerator * Step);
      pragma Assert ((Step * Q - X) * Divisor >= 0.0);
   end Lemma_Quotient_Bound;

   procedure Lemma_Real_Shift (Residue, Z1, Z2, Square : Real)
   with
     Ghost,
     Pre  => Square > 0.0 and then Z1 * Z2 >= Square,
     Post =>
       Z1 /= 0.0 and then Z2 /= 0.0 and then abs (Residue / Z1 - Residue / Z2) * Square <= abs Residue * abs (Z1 - Z2);
   --  r/z₁ - r/z₂ = r·(z₂ - z₁)/(z₁·z₂).

   procedure Lemma_Real_Shift (Residue, Z1, Z2, Square : Real) is
   begin
      pragma Assert (Z1 /= 0.0 and then Z2 /= 0.0);
      declare
         T1 : constant Real := Residue / Z1;
         T2 : constant Real := Residue / Z2;
      begin
         pragma Assert (T1 * Z1 = Residue);
         pragma Assert (T2 * Z2 = Residue);
         pragma Assert ((T1 - T2) * (Z1 * Z2) = (T1 * Z1) * Z2 - (T2 * Z2) * Z1);
         pragma Assert ((T1 - T2) * (Z1 * Z2) = Residue * (Z2 - Z1));
         pragma Assert (abs ((T1 - T2) * (Z1 * Z2)) = abs (T1 - T2) * (Z1 * Z2));
         pragma Assert (abs (Residue * (Z2 - Z1)) = abs Residue * abs (Z1 - Z2));
         Lemma_Scale (Square, Z1 * Z2, abs (T1 - T2));
      end;
   end Lemma_Real_Shift;

   procedure Lemma_Pair_Shift_Weight (G, Square, Product : Real)
   with
     Ghost,
     Pre  => Square > 0.0 and then G * G <= Product and then Square * Square <= Product,
     Post => abs G * Square <= Product;
   --  G² ≤ P and S² ≤ P give |G|·S ≤ P.

   procedure Lemma_Pair_Shift_Weight (G, Square, Product : Real) is
   begin
      pragma Assert (G * G >= 0.0);
      pragma Assert (Product >= 0.0);
      Lemma_Scale (G * G, Product, Square * Square);
      Lemma_Scale (Square * Square, Product, Product);
      pragma Assert ((G * G) * (Square * Square) <= Product * Product);
      pragma Assert ((abs G * Square) * (abs G * Square) = (G * G) * (Square * Square));
      Lemma_Square_Order (abs G * Square, Product);
   end Lemma_Pair_Shift_Weight;

   procedure Lemma_Pair_Shift (Rr, Ri, B, Z1, Z2, Square : Real)
   with
     Ghost,
     Pre  => Square > 0.0 and then Square <= Z1 * Z1 + B * B and then Square <= Z2 * Z2 + B * B,
     Post =>
       abs ((Rr * Z1 - Ri * B) / (Z1 * Z1 + B * B) - (Rr * Z2 - Ri * B) / (Z2 * Z2 + B * B)) * Square
       <= (abs Rr + abs Ri) * abs (Z1 - Z2);
   --  With D = z² + B², both z₁/D₁ - z₂/D₂ = (z₁ - z₂)·(B² - z₁·z₂)/(D₁·D₂) and
   --  B/D₁ - B/D₂ = -(z₁ - z₂)·B·(z₁ + z₂)/(D₁·D₂), and (B² - z₁·z₂)² + B²·(z₁ + z₂)² = D₁·D₂.

   procedure Lemma_Pair_Shift (Rr, Ri, B, Z1, Z2, Square : Real) is
      D1 : constant Real := Z1 * Z1 + B * B;
      D2 : constant Real := Z2 * Z2 + B * B;
      P  : constant Real := D1 * D2;
      T1 : constant Real := (Rr * Z1 - Ri * B) / D1;
      T2 : constant Real := (Rr * Z2 - Ri * B) / D2;
      G1 : constant Real := B * B - Z1 * Z2;
      G2 : constant Real := B * (Z1 + Z2);
      Dz : constant Real := abs (Z1 - Z2);
      Dt : constant Real := abs (T1 - T2);
   begin
      pragma Assert (D1 > 0.0 and then D2 > 0.0);
      pragma Assert (P > 0.0);
      pragma Assert (T1 * D1 = Rr * Z1 - Ri * B);
      pragma Assert (T2 * D2 = Rr * Z2 - Ri * B);
      pragma Assert ((T1 - T2) * P = (T1 * D1) * D2 - (T2 * D2) * D1);
      pragma Assert ((Rr * Z1 - Ri * B) * D2 - (Rr * Z2 - Ri * B) * D1 = (Z1 - Z2) * (Rr * G1 + Ri * G2));
      pragma Assert ((T1 - T2) * P = (Z1 - Z2) * (Rr * G1 + Ri * G2));
      pragma Assert (G1 * G1 + G2 * G2 = P);
      pragma Assert (G2 * G2 >= 0.0 and then G1 * G1 >= 0.0);
      Lemma_Scale (Square, D1, Square);
      Lemma_Scale (Square, D2, D1);
      pragma Assert (Square * Square <= P);
      Lemma_Pair_Shift_Weight (G1, Square, P);
      Lemma_Pair_Shift_Weight (G2, Square, P);
      pragma Assert (abs ((T1 - T2) * P) = Dt * P);
      pragma Assert (abs ((Z1 - Z2) * (Rr * G1 + Ri * G2)) = Dz * abs (Rr * G1 + Ri * G2));
      pragma Assert (abs (Rr * G1) = abs Rr * abs G1);
      pragma Assert (abs (Ri * G2) = abs Ri * abs G2);
      pragma Assert (abs (Rr * G1 + Ri * G2) <= abs Rr * abs G1 + abs Ri * abs G2);
      Lemma_Scale (abs G1 * Square, P, abs Rr);
      Lemma_Scale (abs G2 * Square, P, abs Ri);
      pragma
        Assert
          ((abs Rr * abs G1 + abs Ri * abs G2) * Square = abs Rr * (abs G1 * Square) + abs Ri * (abs G2 * Square));
      pragma Assert ((abs Rr * abs G1 + abs Ri * abs G2) * Square <= (abs Rr + abs Ri) * P);
      Lemma_Scale (abs (Rr * G1 + Ri * G2), abs Rr * abs G1 + abs Ri * abs G2, Square);
      pragma Assert (abs (Rr * G1 + Ri * G2) * Square <= (abs Rr + abs Ri) * P);
      Lemma_Scale (abs (Rr * G1 + Ri * G2) * Square, (abs Rr + abs Ri) * P, Dz);
      pragma Assert ((Dt * P) * Square = (Dz * abs (Rr * G1 + Ri * G2)) * Square);
      pragma Assert ((Dt * Square) * P <= ((abs Rr + abs Ri) * Dz) * P);
      Lemma_Cancel (Dt * Square, (abs Rr + abs Ri) * Dz, P);
   end Lemma_Pair_Shift;

   procedure Lemma_Horizontal_Reach (Pole, V : Real)
   with
     Ghost,
     Pre  => V >= 0.0 and then V <= 1.0,
     Post =>
       Poles.Horizontal_Distance (Pole) >= 0.0
       and then abs (V - Pole) >= Poles.Horizontal_Distance (Pole)
       and then (V - Pole) * (V - Pole) >= Poles.Horizontal_Distance (Pole) * Poles.Horizontal_Distance (Pole);
   --  A point of [0, 1] is at least as far from a real part as [0, 1] itself is.

   procedure Lemma_Horizontal_Reach (Pole, V : Real) is
      H : constant Real := Poles.Horizontal_Distance (Pole);
   begin
      pragma Assert (H >= 0.0);
      pragma Assert (abs (V - Pole) >= H);
      Lemma_Product_Floor (V - Pole, V - Pole, H, H);
      pragma Assert (abs ((V - Pole) * (V - Pole)) = (V - Pole) * (V - Pole));
   end Lemma_Horizontal_Reach;

   procedure Lemma_Term_Shift (Term : Pole_Term; Axis : Axis_Name; V1, V2 : Real)
   with
     Ghost,
     Pre  => Is_Term_Ready (Term) and then V1 >= 0.0 and then V1 <= 1.0 and then V2 >= 0.0 and then V2 <= 1.0,
     Post =>
       Poles.Slope_Term (Term, Axis) >= 0.0
       and then
         abs (Term_Value (Term, Axis, V1) - Term_Value (Term, Axis, V2))
         <= abs (V1 - V2) * Poles.Slope_Term (Term, Axis);
   --  On [0, 1] the term of a pole changes no faster than |r|/d².

   procedure Lemma_Term_Shift (Term : Pole_Term; Axis : Axis_Name; V1, V2 : Real) is
      A      : constant Real := R (Term.Real_Part);
      B      : constant Real := R (Term.Imaginary_Part);
      Rr     : constant Real := R (Term.Residue_Real (Axis));
      Ri     : constant Real := R (Term.Residue_Imaginary (Axis));
      H      : constant Real := Poles.Horizontal_Distance (A);
      Square : constant Real := Poles.Distance_Square (Term);
      Z1     : constant Real := V1 - A;
      Z2     : constant Real := V2 - A;
      Change : constant Real := abs (Term_Value (Term, Axis, V1) - Term_Value (Term, Axis, V2));
      Slope  : constant Real := Poles.Slope_Term (Term, Axis);
   begin
      pragma Assert (Square >= Smallest_Square);
      pragma Assert (Square = H * H + B * B);
      pragma Assert (Slope = (abs Rr + abs Ri) / Square);
      pragma Assert (Slope * Square = abs Rr + abs Ri);
      pragma Assert (Slope >= 0.0);
      Lemma_Horizontal_Reach (A, V1);
      Lemma_Horizontal_Reach (A, V2);
      if Term.Imaginary_Part = 0.0 then
         pragma Assert (B = 0.0);
         pragma Assert (Square = H * H);
         if Term.Real_Part < 0.0 then
            pragma Assert (A < 0.0);
            pragma Assert (Z1 >= H and then Z2 >= H);
            Lemma_Scale (H, Z1, Z2);
            Lemma_Scale (H, Z2, H);
         else
            pragma Assert (A > 1.0);
            pragma Assert (-Z1 >= H and then -Z2 >= H);
            Lemma_Scale (H, -Z1, -Z2);
            Lemma_Scale (H, -Z2, H);
            pragma Assert ((-Z1) * (-Z2) = Z1 * Z2);
         end if;
         pragma Assert (Z1 * Z2 >= Square);
         Lemma_Real_Shift (Rr, Z1, Z2, Square);
         pragma Assert (Term_Value (Term, Axis, V1) = Rr / Z1);
         pragma Assert (Term_Value (Term, Axis, V2) = Rr / Z2);
         pragma Assert (Change * Square <= abs Rr * abs (Z1 - Z2));
         Lemma_Scale (abs Rr, abs Rr + abs Ri, abs (Z1 - Z2));
      else
         pragma Assert (B > 0.0);
         pragma Assert (B * B > 0.0);
         pragma Assert (Z1 * Z1 + B * B >= Square);
         pragma Assert (Z2 * Z2 + B * B >= Square);
         Lemma_Pair_Shift (Rr, Ri, B, Z1, Z2, Square);
         pragma Assert (Term_Value (Term, Axis, V1) = (Rr * Z1 - Ri * B) / (Z1 * Z1 + B * B));
         pragma Assert (Term_Value (Term, Axis, V2) = (Rr * Z2 - Ri * B) / (Z2 * Z2 + B * B));
      end if;
      pragma Assert (Z1 - Z2 = V1 - V2);
      pragma Assert (Change * Square <= (abs Rr + abs Ri) * abs (V1 - V2));
      Lemma_Quotient_Bound (Change, abs Rr + abs Ri, Square, abs (V1 - V2));
   end Lemma_Term_Shift;

   procedure Lemma_Term_Sum_Shift
     (Cache : Rational_Antiderivative; Prepared : Preparation; Count : Natural; Axis : Axis_Name; V1, V2 : Real)
   with
     Ghost,
     Pre                =>
       Has_Layout (Cache)
       and then Poles.Is_Cache_Prepared (Cache, Prepared)
       and then Count <= Prepared.Count
       and then V1 >= 0.0
       and then V1 <= 1.0
       and then V2 >= 0.0
       and then V2 <= 1.0,
     Post               =>
       Poles.Slope_Sum (Prepared, Count, Axis) >= 0.0
       and then
         abs (Term_Sum (Prepared, Count, Axis, V1) - Term_Sum (Prepared, Count, Axis, V2))
         <= abs (V1 - V2) * Poles.Slope_Sum (Prepared, Count, Axis),
     Subprogram_Variant => (Decreases => Count);
   --  The same for the sum of the terms.

   procedure Lemma_Term_Sum_Shift
     (Cache : Rational_Antiderivative; Prepared : Preparation; Count : Natural; Axis : Axis_Name; V1, V2 : Real) is
   begin
      if Count = 0 then
         pragma Assert (Term_Sum (Prepared, 0, Axis, V1) = 0.0);
         pragma Assert (Term_Sum (Prepared, 0, Axis, V2) = 0.0);
         pragma Assert (Poles.Slope_Sum (Prepared, 0, Axis) = 0.0);
         return;
      end if;
      Lemma_Term_Sum_Shift (Cache, Prepared, Count - 1, Axis, V1, V2);
      Lemma_Term_Kind (Cache, Prepared, Count);
      pragma Assert (Is_Term_Ready (Prepared.Terms (Count)));
      Lemma_Term_Shift (Prepared.Terms (Count), Axis, V1, V2);
      pragma
        Assert
          (Term_Sum (Prepared, Count, Axis, V1)
           = Term_Sum (Prepared, Count - 1, Axis, V1) + Term_Value (Prepared.Terms (Count), Axis, V1));
      pragma
        Assert
          (Term_Sum (Prepared, Count, Axis, V2)
           = Term_Sum (Prepared, Count - 1, Axis, V2) + Term_Value (Prepared.Terms (Count), Axis, V2));
      pragma
        Assert
          (Poles.Slope_Sum (Prepared, Count, Axis)
           = Poles.Slope_Sum (Prepared, Count - 1, Axis) + Poles.Slope_Term (Prepared.Terms (Count), Axis));
      pragma
        Assert
          (abs (V1 - V2) * Poles.Slope_Sum (Prepared, Count, Axis)
           = abs (V1 - V2) * Poles.Slope_Sum (Prepared, Count - 1, Axis)
             + abs (V1 - V2) * Poles.Slope_Term (Prepared.Terms (Count), Axis));
   end Lemma_Term_Sum_Shift;

   ---------------------------------------------------------------------------
   --  Accumulating the terms of the retained tangent
   ---------------------------------------------------------------------------

   Big : constant := 2.0**200;
   --  More than Magnitude_Limit.

   Term_Error : constant := 9.5 * 2.0**(-53);
   --  A computed term is within this times its magnitude of the exact term at the computed distance,

   Zeta : constant := 2.0**(-600);
   --  this times the pole's share of the slope, for an underflow which a residue amplifies,

   Term_Dust : constant := 3.1 * 2.0**(-1075);
   --  and this, for its own underflow.

   Dust_Step : constant := 2.0**(-1072);
   --  The underflow which one term can add to a sum.

   Half_Term_Limit : constant := 2.0**605;
   Term_Limit      : constant := 2.0**606;
   --  No term or its magnitude exceeds the first, so that no partial sum exceeds its number of terms times the
   --  second.

   Sum_Limit : constant := 2.0**610;
   --  Sixteen times Term_Limit: no partial sum is larger.

   function Alpha (Count : Natural) return Real
   is ((10.5 + 1.1 * To_Real (Count)) * Epsilon)
   with Ghost;
   --  The sum of Count rounded terms is within this times the sum of their magnitudes of the exact sum,

   function Gamma (Count : Natural) return Real
   is (1.0 + (4.0 + 2.0 * To_Real (Count)) * Epsilon)
   with Ghost;
   --  is itself within this times the sum of the magnitudes, which the computed sum of magnitudes is within
   --  the same factor of,

   function Dust (Count : Natural) return Real
   is (To_Real (Count) * Dust_Step)
   with Ghost;
   --  all up to this much underflow.

   procedure Lemma_Coefficients (Count : Positive)
   with
     Ghost,
     Pre  => Count <= Maximum_Rational_Degree,
     Post =>
       To_Real (Count) = To_Real (Count - 1) + 1.0
       and then To_Real (Count - 1) >= 0.0
       and then To_Real (Count) <= 16.0
       and then Gamma (Count - 1) >= 1.0 + 4.0 * Epsilon
       and then Gamma (Count) = Gamma (Count - 1) + 2.0 * Epsilon
       and then Gamma (Count) <= 1.0 + 36.0 * Epsilon
       and then Alpha (Count - 1) >= 0.0
       and then Alpha (Count) = Alpha (Count - 1) + 1.1 * Epsilon
       and then Alpha (Count) >= 11.6 * Epsilon
       and then Alpha (Count) <= 28.1 * Epsilon
       and then Dust (Count - 1) >= 0.0
       and then Dust (Count - 1) <= 120.0 * Eta
       and then Dust (Count) = Dust (Count - 1) + 8.0 * Eta
       and then Dust (Count) <= 128.0 * Eta;

   procedure Lemma_Coefficients (Count : Positive) is
   begin
      pragma Assert (To_Real (Count) = To_Real (Count - 1) + 1.0);
      pragma Assert (To_Real (Count - 1) >= 0.0);
      pragma Assert (To_Real (Count) <= 16.0);
      pragma Assert (Dust_Step = 8.0 * Eta);
   end Lemma_Coefficients;

   procedure Lemma_Accumulate_Magnitude (Ret, T_Hat, Ret_New, M, Mu, G_Old, G_New, D_Old, D_New : Real)
   with
     Ghost,
     Pre  =>
       M >= 0.0
       and then Mu >= 0.0
       and then abs Ret <= G_Old * M + D_Old
       and then abs T_Hat <= (1.0 + 2.1 * Epsilon) * Mu + Term_Dust
       and then abs (Ret_New - (Ret + T_Hat)) <= Epsilon * abs (Ret + T_Hat) + Eta
       and then G_Old >= 1.0 + 4.0 * Epsilon
       and then G_Old <= 1.0 + 40.0 * Epsilon
       and then G_New >= G_Old + 2.0 * Epsilon
       and then D_Old >= 0.0
       and then D_Old <= 128.0 * Eta
       and then D_New >= D_Old + 8.0 * Eta,
     Post =>
       abs (Ret + T_Hat) <= (1.0 + 40.0 * Epsilon) * M + D_Old + (1.0 + 2.1 * Epsilon) * Mu + Term_Dust
       and then abs Ret_New <= G_New * (M + Mu) + D_New;
   --  The magnitude of a rounded partial sum, against the sum of the magnitudes of its terms.

   procedure Lemma_Accumulate_Magnitude (Ret, T_Hat, Ret_New, M, Mu, G_Old, G_New, D_Old, D_New : Real) is
      GM    : constant Real := G_Old * M;
      Bound : constant Real := GM + D_Old + (1.0 + 2.1 * Epsilon) * Mu + Term_Dust;
   begin
      Lemma_Scale (G_Old, 1.0 + 40.0 * Epsilon, M);
      pragma Assert (GM <= (1.0 + 40.0 * Epsilon) * M);
      pragma Assert (abs (Ret + T_Hat) <= Bound);
      pragma Assert (abs Ret_New <= (1.0 + Epsilon) * Bound + Eta);
      pragma Assert (Epsilon * GM <= 1.1 * Epsilon * M);
      Lemma_Scale (G_Old + 2.0 * Epsilon, G_New, M);
      pragma Assert ((G_Old + 2.0 * Epsilon) * M = GM + 2.0 * Epsilon * M);
      pragma Assert ((1.0 + Epsilon) * GM <= G_New * M);
      Lemma_Scale (1.0 + 6.0 * Epsilon, G_New, Mu);
      pragma Assert ((1.0 + Epsilon) * ((1.0 + 2.1 * Epsilon) * Mu) <= (1.0 + 6.0 * Epsilon) * Mu);
      pragma Assert ((1.0 + Epsilon) * ((1.0 + 2.1 * Epsilon) * Mu) <= G_New * Mu);
      pragma Assert ((1.0 + Epsilon) * (D_Old + Term_Dust) + Eta <= D_New);
      pragma
        Assert
          ((1.0 + Epsilon) * Bound + Eta
           = (1.0 + Epsilon) * GM
             + (1.0 + Epsilon) * ((1.0 + 2.1 * Epsilon) * Mu)
             + ((1.0 + Epsilon) * (D_Old + Term_Dust) + Eta));
      pragma Assert (G_New * (M + Mu) = G_New * M + G_New * Mu);
   end Lemma_Accumulate_Magnitude;

   procedure Lemma_Accumulate_Error (Ret, T_Hat, Ret_New, Exact, Tau, M, Mu, SS, Sl, A_Old, A_New, D_Old, D_New : Real)
   with
     Ghost,
     Pre  =>
       M >= 0.0
       and then Mu >= 0.0
       and then abs (Ret - Exact) <= A_Old * M + Zeta * SS + D_Old
       and then abs (T_Hat - Tau) <= Term_Error * Mu + Zeta * Sl + Term_Dust
       and then abs (Ret + T_Hat) <= (1.0 + 40.0 * Epsilon) * M + D_Old + (1.0 + 2.1 * Epsilon) * Mu + Term_Dust
       and then abs (Ret_New - (Ret + T_Hat)) <= Epsilon * abs (Ret + T_Hat) + Eta
       and then A_Old >= 0.0
       and then A_New >= A_Old + 1.1 * Epsilon
       and then A_New >= 11.6 * Epsilon
       and then D_Old >= 0.0
       and then D_Old <= 128.0 * Eta
       and then D_New >= D_Old + 8.0 * Eta,
     Post => abs (Ret_New - (Exact + Tau)) <= A_New * (M + Mu) + Zeta * (SS + Sl) + D_New;
   --  The error of a rounded partial sum: that of the sum before, that of the new term, and one rounding of
   --  their sum.

   procedure Lemma_Accumulate_Error (Ret, T_Hat, Ret_New, Exact, Tau, M, Mu, SS, Sl, A_Old, A_New, D_Old, D_New : Real)
   is
      AM : constant Real := A_Old * M;
   begin
      pragma
        Assert
          (abs (Ret_New - (Exact + Tau)) <= abs (Ret_New - (Ret + T_Hat)) + abs (Ret - Exact) + abs (T_Hat - Tau));
      pragma
        Assert
          (abs (Ret_New - (Ret + T_Hat))
           <= 1.01 * Epsilon * M + 1.01 * Epsilon * Mu + Epsilon * (D_Old + Term_Dust) + Eta);
      Lemma_Scale (A_Old + 1.1 * Epsilon, A_New, M);
      pragma Assert ((A_Old + 1.1 * Epsilon) * M = AM + 1.1 * Epsilon * M);
      pragma Assert (AM + 1.01 * Epsilon * M <= A_New * M);
      Lemma_Scale (11.6 * Epsilon, A_New, Mu);
      pragma Assert (Term_Error * Mu + 1.01 * Epsilon * Mu <= A_New * Mu);
      pragma Assert (Epsilon * (D_Old + Term_Dust) + Eta + D_Old + Term_Dust <= D_New);
      pragma Assert (A_New * (M + Mu) = A_New * M + A_New * Mu);
      pragma Assert (Zeta * (SS + Sl) = Zeta * SS + Zeta * Sl);
   end Lemma_Accumulate_Error;

   procedure Lemma_Accumulate_Size (Size, S_Hat, Size_New, M, Mu, G_Old, G_New, D_Old, D_New : Real)
   with
     Ghost,
     Pre  =>
       M >= 0.0
       and then Mu >= 0.0
       and then Size >= 0.0
       and then S_Hat >= 0.0
       and then M <= G_Old * Size + D_Old
       and then S_Hat >= (1.0 - 2.1 * Epsilon) * Mu - Term_Dust
       and then abs (Size_New - (Size + S_Hat)) <= Epsilon * abs (Size + S_Hat) + Eta
       and then G_Old >= 1.0 + 4.0 * Epsilon
       and then G_New >= G_Old + 2.0 * Epsilon
       and then G_New <= 1.0 + 40.0 * Epsilon
       and then D_Old >= 0.0
       and then D_New >= D_Old + 8.0 * Eta,
     Post => M + Mu <= G_New * Size_New + D_New;
   --  The sum of the magnitudes, against the rounded sum of their rounded values.

   procedure Lemma_Accumulate_Size (Size, S_Hat, Size_New, M, Mu, G_Old, G_New, D_Old, D_New : Real) is
      pragma Unreferenced (D_New);
      Sum   : constant Real := Size + S_Hat;
      Upper : constant Real := (1.0 + 1.01 * Epsilon) * (Size_New + Eta);
   begin
      pragma Assert ((1.0 - 2.1 * Epsilon) * Mu <= S_Hat + Term_Dust);
      pragma Assert (Mu <= (1.0 + 2.2 * Epsilon) * (S_Hat + Term_Dust));
      Lemma_Scale (1.0 + 2.2 * Epsilon, G_Old, S_Hat);
      pragma Assert (G_Old * Sum = G_Old * Size + G_Old * S_Hat);
      pragma Assert (M + Mu <= G_Old * Sum + D_Old + 3.2 * Eta);
      pragma Assert ((1.0 - Epsilon) * Sum <= Size_New + Eta);
      pragma Assert (Size_New + Eta >= 0.0);
      pragma Assert (Sum <= Upper);
      Lemma_Scale (Sum, Upper, G_Old);
      pragma Assert (G_Old * Upper = (G_Old * (1.0 + 1.01 * Epsilon)) * (Size_New + Eta));
      pragma Assert (G_Old * (1.0 + 1.01 * Epsilon) <= G_Old + 2.0 * Epsilon);
      Lemma_Scale (G_Old * (1.0 + 1.01 * Epsilon), G_New, Size_New + Eta);
      pragma Assert (G_Old * Sum <= G_New * (Size_New + Eta));
      pragma Assert (G_New * (Size_New + Eta) = G_New * Size_New + G_New * Eta);
      pragma Assert (G_New * Eta <= 2.0 * Eta);
   end Lemma_Accumulate_Size;

   function Is_Axis_State
     (Prepared : Preparation; Count : Natural; Axis : Axis_Name; U, Ret, Size : Dimensionless; M : Real) return Boolean
   with
     Ghost,
     Pre  => Count <= Maximum_Rational_Degree,
     Post =>
       Is_Axis_State'Result
       = (Size >= 0.0
          and then M >= 0.0
          and then abs (R (Ret)) <= To_Real (Count) * Term_Limit
          and then R (Size) <= To_Real (Count) * Term_Limit
          and then
            (if Prepared.Active (Axis)
             then
               abs (R (Ret) - Term_Sum (Prepared, Count, Axis, R (U)))
               <= Alpha (Count) * M + Zeta * Poles.Slope_Sum (Prepared, Count, Axis) + Dust (Count)
               and then abs (R (Ret)) <= Gamma (Count) * M + Dust (Count)
               and then M <= Gamma (Count) * R (Size) + Dust (Count)));
   --  What the node loop maintains on one axis after Count poles, with M the exact sum of the magnitudes of the
   --  computed terms: the bounds which keep the sums finite and, on an active axis, the accuracy of the computed
   --  sum Ret against the exact terms at the computed distance and of the computed sum of magnitudes Size.

   function Is_Axis_State
     (Prepared : Preparation; Count : Natural; Axis : Axis_Name; U, Ret, Size : Dimensionless; M : Real) return Boolean
   is
   begin
      return
        Size >= 0.0
        and then M >= 0.0
        and then abs (R (Ret)) <= To_Real (Count) * Term_Limit
        and then R (Size) <= To_Real (Count) * Term_Limit
        and then
          (if Prepared.Active (Axis)
           then
             abs (R (Ret) - Term_Sum (Prepared, Count, Axis, R (U)))
             <= Alpha (Count) * M + Zeta * Poles.Slope_Sum (Prepared, Count, Axis) + Dust (Count)
             and then abs (R (Ret)) <= Gamma (Count) * M + Dust (Count)
             and then M <= Gamma (Count) * R (Size) + Dust (Count));
   end Is_Axis_State;

   procedure Lemma_Sum_Step (Sum, Term : Dimensionless; Count : Positive)
   with
     Ghost,
     Pre  =>
       Count <= Maximum_Rational_Degree
       and then abs (R (Sum)) <= To_Real (Count - 1) * Term_Limit
       and then abs (R (Term)) <= Half_Term_Limit,
     Post =>
       abs Sum <= 2.0**610
       and then abs Term <= 2.0**605
       and then abs (Sum + Term) <= 2.0**610
       and then abs (R (Sum + Term)) <= To_Real (Count) * Term_Limit
       and then abs (R (Sum + Term) - (R (Sum) + R (Term))) <= Epsilon * abs (R (Sum) + R (Term)) + Eta;
   --  One more term keeps a partial sum within its limit.

   procedure Lemma_Sum_Step (Sum, Term : Dimensionless; Count : Positive) is
   begin
      Lemma_Coefficients (Count);
      pragma Assert (abs (R (Sum)) <= 15.0 * Term_Limit);
      pragma Assert (abs Sum <= 2.0**610);
      pragma Assert (abs Term <= 2.0**605);
      Float_Lemmas.Lemma_Rounding_Error_Add (Sum, Term);
      pragma Assert (abs (R (Sum) + R (Term)) <= To_Real (Count - 1) * Term_Limit + Half_Term_Limit);
      pragma Assert (Epsilon * abs (R (Sum) + R (Term)) + Eta <= Half_Term_Limit);
      pragma Assert (abs (R (Sum + Term)) <= To_Real (Count) * Term_Limit);
      pragma Assert (abs (R (Sum + Term)) <= Sum_Limit);
   end Lemma_Sum_Step;

   procedure Lemma_Idle_Axis
     (Prepared : Preparation; Count : Positive; Axis : Axis_Name; U, Ret, Size : Dimensionless; M : Real)
   with
     Ghost,
     Pre  => Count <= Maximum_Rational_Degree and then Is_Axis_State (Prepared, Count - 1, Axis, U, Ret, Size, M),
     Post => (if not Prepared.Active (Axis) then Is_Axis_State (Prepared, Count, Axis, U, Ret, Size, M));
   --  An axis which is not active keeps its state.

   procedure Lemma_Idle_Axis
     (Prepared : Preparation; Count : Positive; Axis : Axis_Name; U, Ret, Size : Dimensionless; M : Real)
   is
      pragma Unreferenced (Prepared, Axis, U, Ret, Size, M);
   begin
      Lemma_Coefficients (Count);
      pragma Assert (To_Real (Count - 1) * Term_Limit <= To_Real (Count) * Term_Limit);
   end Lemma_Idle_Axis;

   procedure Lemma_Slope_Term_Sign (Term : Pole_Term; Axis : Axis_Name)
   with
     Ghost,
     Pre  => Poles.Is_Term_Bounded (Term),
     Post =>
       Poles.Slope_Term (Term, Axis) >= 0.0
       and then
         Poles.Slope_Term (Term, Axis) * Poles.Distance_Square (Term)
         = abs (R (Term.Residue_Real (Axis))) + abs (R (Term.Residue_Imaginary (Axis)));

   procedure Lemma_Slope_Term_Sign (Term : Pole_Term; Axis : Axis_Name) is
      Square : constant Real := Poles.Distance_Square (Term);
      Slope  : constant Real := Poles.Slope_Term (Term, Axis);
   begin
      pragma Assert (Square >= Smallest_Square);
      pragma Assert (Slope = (abs (R (Term.Residue_Real (Axis))) + abs (R (Term.Residue_Imaginary (Axis)))) / Square);
      pragma Assert (Slope * Square = abs (R (Term.Residue_Real (Axis))) + abs (R (Term.Residue_Imaginary (Axis))));
      pragma Assert (Slope >= 0.0);
   end Lemma_Slope_Term_Sign;

   procedure Lemma_Nonnegative (Value : Dimensionless)
   with Ghost, Pre => Value >= 0.0, Post => R (Value) >= 0.0 and then abs (R (Value)) = R (Value);

   procedure Lemma_Nonnegative (Value : Dimensionless) is null;

   procedure Lemma_Sum_Steps (Prepared : Preparation; Index : Rational_Degree_Slot; Axis : Axis_Name; V : Real)
   with
     Ghost,
     Post =>
       Term_Sum (Prepared, Index, Axis, V)
       = Term_Sum (Prepared, Index - 1, Axis, V) + Term_Value (Prepared.Terms (Index), Axis, V)
       and then
         Poles.Slope_Sum (Prepared, Index, Axis)
         = Poles.Slope_Sum (Prepared, Index - 1, Axis) + Poles.Slope_Term (Prepared.Terms (Index), Axis);
   --  One more pole in the two sums over the gathered poles.

   procedure Lemma_Sum_Steps (Prepared : Preparation; Index : Rational_Degree_Slot; Axis : Axis_Name; V : Real) is
   begin
      pragma
        Assert
          (Term_Sum (Prepared, Index, Axis, V)
           = Term_Sum (Prepared, Index - 1, Axis, V) + Term_Value (Prepared.Terms (Index), Axis, V));
      pragma
        Assert
          (Poles.Slope_Sum (Prepared, Index, Axis)
           = Poles.Slope_Sum (Prepared, Index - 1, Axis) + Poles.Slope_Term (Prepared.Terms (Index), Axis));
   end Lemma_Sum_Steps;

   procedure Lemma_Axis_Step
     (Prepared    : Preparation;
      Index       : Rational_Degree_Slot;
      Axis        : Axis_Name;
      U           : Dimensionless;
      Ret, T_Hat  : Dimensionless;
      Size, S_Hat : Dimensionless;
      M, Mu, Tau  : Real)
   with
     Ghost,
     Pre  =>
       Is_Axis_State (Prepared, Index - 1, Axis, U, Ret, Size, M)
       and then Tau = Term_Value (Prepared.Terms (Index), Axis, R (U))
       and then Poles.Slope_Term (Prepared.Terms (Index), Axis) >= 0.0
       and then Mu >= 0.0
       and then S_Hat >= 0.0
       and then abs (R (T_Hat)) <= Half_Term_Limit
       and then R (S_Hat) <= Half_Term_Limit
       and then
         abs (R (T_Hat) - Tau) <= Term_Error * Mu + Zeta * Poles.Slope_Term (Prepared.Terms (Index), Axis) + Term_Dust
       and then abs (R (T_Hat)) <= (1.0 + 2.1 * Epsilon) * Mu + Term_Dust
       and then R (S_Hat) >= (1.0 - 2.1 * Epsilon) * Mu - Term_Dust,
     Post =>
       abs Ret <= 2.0**610
       and then abs T_Hat <= 2.0**605
       and then abs Size <= 2.0**610
       and then abs S_Hat <= 2.0**605
       and then Is_Axis_State (Prepared, Index, Axis, U, Ret + T_Hat, Size + S_Hat, M + Mu);
   --  One pole more on one axis, given how its computed term T_Hat and computed magnitude S_Hat relate to the
   --  exact term Tau and to the exact magnitude Mu of the computed term.

   procedure Lemma_Axis_Step
     (Prepared    : Preparation;
      Index       : Rational_Degree_Slot;
      Axis        : Axis_Name;
      U           : Dimensionless;
      Ret, T_Hat  : Dimensionless;
      Size, S_Hat : Dimensionless;
      M, Mu, Tau  : Real)
   is
      Exact : constant Real := Term_Sum (Prepared, Index - 1, Axis, R (U));
      SS    : constant Real := Poles.Slope_Sum (Prepared, Index - 1, Axis);
      Sl    : constant Real := Poles.Slope_Term (Prepared.Terms (Index), Axis);
   begin
      Lemma_Coefficients (Index);
      Lemma_Sum_Steps (Prepared, Index, Axis, R (U));
      pragma Assert (Term_Sum (Prepared, Index, Axis, R (U)) = Exact + Tau);
      pragma Assert (Poles.Slope_Sum (Prepared, Index, Axis) = SS + Sl);
      Lemma_Nonnegative (Size);
      Lemma_Nonnegative (S_Hat);
      pragma Assert (abs (R (Ret)) <= To_Real (Index - 1) * Term_Limit);
      pragma Assert (abs (R (Size)) <= To_Real (Index - 1) * Term_Limit);
      pragma Assert (abs (R (S_Hat)) <= Half_Term_Limit);
      Lemma_Sum_Step (Ret, T_Hat, Index);
      Lemma_Sum_Step (Size, S_Hat, Index);
      pragma Assert (Size + S_Hat >= 0.0);
      Lemma_Nonnegative (Size + S_Hat);
      pragma Assert (R (Size + S_Hat) <= To_Real (Index) * Term_Limit);
      pragma Assert (M + Mu >= 0.0);
      if Prepared.Active (Axis) then
         pragma Assert (abs (R (Ret) - Exact) <= Alpha (Index - 1) * M + Zeta * SS + Dust (Index - 1));
         pragma Assert (abs (R (Ret)) <= Gamma (Index - 1) * M + Dust (Index - 1));
         pragma Assert (M <= Gamma (Index - 1) * R (Size) + Dust (Index - 1));
         Lemma_Accumulate_Magnitude
           (R (Ret),
            R (T_Hat),
            R (Ret + T_Hat),
            M,
            Mu,
            Gamma (Index - 1),
            Gamma (Index),
            Dust (Index - 1),
            Dust (Index));
         Lemma_Accumulate_Error
           (R (Ret),
            R (T_Hat),
            R (Ret + T_Hat),
            Exact,
            Tau,
            M,
            Mu,
            SS,
            Sl,
            Alpha (Index - 1),
            Alpha (Index),
            Dust (Index - 1),
            Dust (Index));
         Lemma_Accumulate_Size
           (R (Size),
            R (S_Hat),
            R (Size + S_Hat),
            M,
            Mu,
            Gamma (Index - 1),
            Gamma (Index),
            Dust (Index - 1),
            Dust (Index));
         pragma
           Assert
             (abs (R (Ret + T_Hat) - Term_Sum (Prepared, Index, Axis, R (U)))
              <= Alpha (Index) * (M + Mu) + Zeta * Poles.Slope_Sum (Prepared, Index, Axis) + Dust (Index));
         pragma Assert (abs (R (Ret + T_Hat)) <= Gamma (Index) * (M + Mu) + Dust (Index));
         pragma Assert (M + Mu <= Gamma (Index) * R (Size + S_Hat) + Dust (Index));
      end if;
   end Lemma_Axis_Step;

   ---------------------------------------------------------------------------
   --  The term of a real pole
   ---------------------------------------------------------------------------

   Reciprocal_Error : constant := 2.01 * 2.0**(-53);
   --  The relative error of the computed 1/(U - p): the rounded difference and the rounded quotient.

   procedure Lemma_Limit (Value : Dimensionless)
   with Ghost, Pre => abs Value <= Magnitude_Limit, Post => abs (R (Value)) <= Big;

   procedure Lemma_Limit (Value : Dimensionless) is null;

   procedure Lemma_Abs (Value : Dimensionless)
   with Ghost, Post => R (abs Value) = abs (R (Value));

   procedure Lemma_Abs (Value : Dimensionless) is null;

   procedure Lemma_Real_Pole (U, Pole, X : Dimensionless)
   with
     Ghost,
     Pre  =>
       U >= 0.0
       and then U <= 1.0
       and then abs Pole <= Magnitude_Limit
       and then (Pole < 0.0 or else Pole > 1.0)
       and then Poles.Horizontal_Distance (R (Pole)) * Poles.Horizontal_Distance (R (Pole)) >= Smallest_Square
       and then abs (R (X) - (R (U) - R (Pole))) <= Epsilon * abs (R (U) - R (Pole)) + Eta,
     Post =>
       abs X >= 2.0**(-101)
       and then abs X <= 2.0**201
       and then X /= 0.0
       and then abs (1.0 / X) <= 2.0**101
       and then abs (R (U) - R (Pole)) >= Reach
       and then abs (R (X) - (R (U) - R (Pole))) <= Unit * abs (R (U) - R (Pole))
       and then abs (R (1.0 / X) - 1.0 / (R (U) - R (Pole))) <= Reciprocal_Error * abs (1.0 / (R (U) - R (Pole)));
   --  X is the rounded difference of the computed distance and a real pole. The pole keeps the difference away
   --  from zero, and the reciprocal of the difference is accurate.

   procedure Lemma_Real_Pole (U, Pole, X : Dimensionless) is
      Z : constant Real := R (U) - R (Pole);
      H : constant Real := Poles.Horizontal_Distance (R (Pole));
   begin
      Lemma_Limit (Pole);
      pragma Assert (H >= 0.0);
      pragma Assert (Reach * Reach = Smallest_Square);
      Lemma_Square_Order (Reach, H);
      pragma Assert (abs Z >= H);
      pragma Assert (abs Z >= Reach);
      pragma Assert (abs Z <= 1.0 + Big);
      Lemma_Normal_Rounding (R (X), Z);
      pragma Assert (abs (R (X)) >= (1.0 - Unit) * Reach);
      pragma Assert (abs (R (X)) <= (1.0 + Unit) * (1.0 + Big));
      pragma Assert (abs X >= 2.0**(-101));
      pragma Assert (abs X <= 2.0**201);
      pragma Assert (X /= 0.0);
      pragma Assert (abs (1.0 / X) <= 2.0**101);
      Float_Lemmas.Lemma_Rounding_Error_Div (1.0, X);
      Lemma_Relative_Reciprocal (R (X), Z, Unit, 1.002 * Epsilon);
      Lemma_Reciprocal_Floor (R (X), 2.0 * (1.0 + Big));
      pragma Assert (abs (1.0 / R (X)) >= Normal_Floor);
      pragma Assert (R (1.0) = 1.0);
      Lemma_Normal_Rounding (R (1.0 / X), 1.0 / R (X));
      Lemma_Relative_Chain (R (1.0 / X), 1.0 / R (X), 1.0 / Z, Unit, 1.002 * Epsilon, Reciprocal_Error);
   end Lemma_Real_Pole;

   procedure Lemma_Real_Pole_Term (U : Dimensionless; Term : Pole_Term; X : Dimensionless)
   with
     Ghost,
     Pre  =>
       U >= 0.0
       and then U <= 1.0
       and then Is_Term_Ready (Term)
       and then Term.Imaginary_Part = 0.0
       and then abs (R (X) - (R (U) - R (Term.Real_Part))) <= Epsilon * abs (R (U) - R (Term.Real_Part)) + Eta,
     Post =>
       abs X >= 2.0**(-101)
       and then abs X <= 2.0**201
       and then X /= 0.0
       and then abs (1.0 / X) <= 2.0**101
       and then abs (R (U) - R (Term.Real_Part)) >= Reach
       and then R (U) - R (Term.Real_Part) /= 0.0
       and then abs (R (X) - (R (U) - R (Term.Real_Part))) <= Unit * abs (R (U) - R (Term.Real_Part))
       and then
         abs (R (1.0 / X) - 1.0 / (R (U) - R (Term.Real_Part)))
         <= Reciprocal_Error * abs (1.0 / (R (U) - R (Term.Real_Part)));
   --  The same, for a gathered real pole.

   procedure Lemma_Real_Pole_Term (U : Dimensionless; Term : Pole_Term; X : Dimensionless) is
      H : constant Real := Poles.Horizontal_Distance (R (Term.Real_Part));
   begin
      pragma Assert (Poles.Is_Term_Bounded (Term));
      pragma Assert (abs Term.Real_Part <= Magnitude_Limit);
      pragma Assert (R (Term.Imaginary_Part) = 0.0);
      pragma Assert (Poles.Distance_Square (Term) >= Smallest_Square);
      pragma Assert (Poles.Distance_Square (Term) = H * H + R (Term.Imaginary_Part) * R (Term.Imaginary_Part));
      pragma Assert (H * H >= Smallest_Square);
      Lemma_Real_Pole (U, Term.Real_Part, X);
   end Lemma_Real_Pole_Term;

   procedure Lemma_Real_Term (Residue, Rec : Dimensionless; Z : Real)
   with
     Ghost,
     Pre  =>
       abs Residue <= Magnitude_Limit
       and then abs Rec <= 2.0**101
       and then Z /= 0.0
       and then abs (R (Rec) - 1.0 / Z) <= Reciprocal_Error * abs (1.0 / Z),
     Post =>
       abs (Residue * Rec) <= 2.0**302
       and then abs Residue * abs Rec >= 0.0
       and then abs Residue * abs Rec <= 2.0**302
       and then abs (R (Residue)) * abs (R (Rec)) <= Half_Term_Limit
       and then abs (R (Residue * Rec)) <= Half_Term_Limit
       and then R (abs Residue * abs Rec) <= Half_Term_Limit
       and then
         abs (R (Residue * Rec) - R (Residue) / Z) <= Term_Error * (abs (R (Residue)) * abs (R (Rec))) + Term_Dust
       and then abs (R (Residue * Rec)) <= (1.0 + 2.1 * Epsilon) * (abs (R (Residue)) * abs (R (Rec))) + Term_Dust
       and then R (abs Residue * abs Rec) >= (1.0 - 2.1 * Epsilon) * (abs (R (Residue)) * abs (R (Rec))) - Term_Dust;
   --  The rounded term r·fl(1/(U - p)) against r/(U - p), and its rounded magnitude.

   procedure Lemma_Real_Term (Residue, Rec : Dimensionless; Z : Real) is
      Rr  : constant Real := R (Residue);
      Rc  : constant Real := R (Rec);
      Mu  : constant Real := abs Rr * abs Rc;
      Tau : constant Real := Rr * (1.0 / Z);
   begin
      Lemma_Limit (Residue);
      pragma Assert (abs Rc <= 2.0 * Big);
      Lemma_Product_Bound (Rr, Rc, Big, 2.0 * Big);
      pragma Assert (abs (Rr * Rc) = Mu);
      pragma Assert (Mu <= Big * (2.0 * Big));
      pragma Assert (Mu <= 0.5 * Half_Term_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Mul (Residue, Rec);
      Lemma_Abs (Residue);
      Lemma_Abs (Rec);
      Float_Lemmas.Lemma_Rounding_Error_Mul (abs Residue, abs Rec);
      pragma Assert (abs (R (Residue * Rec) - Rr * Rc) <= Epsilon * Mu + Eta);
      pragma Assert (abs (R (abs Residue * abs Rec) - Mu) <= Epsilon * Mu + Eta);
      pragma Assert (abs (R (Residue * Rec)) <= Half_Term_Limit);
      pragma Assert (R (abs Residue * abs Rec) <= Half_Term_Limit);
      pragma Assert (abs (Residue * Rec) <= 2.0**302);
      pragma Assert (abs Residue * abs Rec <= 2.0**302);
      pragma Assert (abs Residue * abs Rec >= 0.0);
      Lemma_Relative_Product (Rr, Rr, Rc, 1.0 / Z, 0.0, Reciprocal_Error, Reciprocal_Error);
      pragma Assert (abs (Rr * Rc - Tau) <= Reciprocal_Error * abs Tau);
      pragma Assert ((1.0 - Reciprocal_Error) * abs Tau <= Mu);
      pragma Assert (Reciprocal_Error * abs Tau <= 2.02 * Epsilon * Mu);
      Lemma_Division (Rr, Z);
      pragma Assert (abs (R (Residue * Rec) - Tau) <= 3.02 * Epsilon * Mu + Eta);
   end Lemma_Real_Term;

   procedure Lemma_Real_Axis
     (Prepared : Preparation;
      Index    : Rational_Degree_Slot;
      Axis     : Axis_Name;
      U, Rec   : Dimensionless;
      Ret      : Dimensionless;
      Size     : Dimensionless;
      M        : Real)
   with
     Ghost,
     Pre  =>
       Is_Term_Ready (Prepared.Terms (Index))
       and then Prepared.Terms (Index).Imaginary_Part = 0.0
       and then abs Rec <= 2.0**101
       and then R (U) - R (Prepared.Terms (Index).Real_Part) /= 0.0
       and then
         abs (R (Rec) - 1.0 / (R (U) - R (Prepared.Terms (Index).Real_Part)))
         <= Reciprocal_Error * abs (1.0 / (R (U) - R (Prepared.Terms (Index).Real_Part)))
       and then Is_Axis_State (Prepared, Index - 1, Axis, U, Ret, Size, M),
     Post =>
       abs (Prepared.Terms (Index).Residue_Real (Axis) * Rec) <= 2.0**302
       and then abs Prepared.Terms (Index).Residue_Real (Axis) * abs Rec <= 2.0**302
       and then abs Ret <= 2.0**610
       and then abs Size <= 2.0**610
       and then
         Is_Axis_State
           (Prepared,
            Index,
            Axis,
            U,
            Ret + Prepared.Terms (Index).Residue_Real (Axis) * Rec,
            Size + abs Prepared.Terms (Index).Residue_Real (Axis) * abs Rec,
            M + abs (R (Prepared.Terms (Index).Residue_Real (Axis))) * abs (R (Rec)));
   --  One real pole more on one axis.

   procedure Lemma_Real_Axis
     (Prepared : Preparation;
      Index    : Rational_Degree_Slot;
      Axis     : Axis_Name;
      U, Rec   : Dimensionless;
      Ret      : Dimensionless;
      Size     : Dimensionless;
      M        : Real)
   is
      Term    : constant Pole_Term := Prepared.Terms (Index);
      Residue : constant Dimensionless := Term.Residue_Real (Axis);
      Z       : constant Real := R (U) - R (Term.Real_Part);
      Mu      : constant Real := abs (R (Residue)) * abs (R (Rec));
      Tau     : constant Real := R (Residue) / Z;
   begin
      pragma Assert (Poles.Is_Term_Bounded (Term));
      pragma Assert (abs Residue <= Magnitude_Limit);
      Lemma_Real_Term (Residue, Rec, Z);
      Lemma_Slope_Term_Sign (Term, Axis);
      pragma Assert (Mu >= 0.0);
      pragma Assert (Term_Value (Term, Axis, R (U)) = Tau);
      Lemma_Axis_Step (Prepared, Index, Axis, U, Ret, Residue * Rec, Size, abs Residue * abs Rec, M, Mu, Tau);
   end Lemma_Real_Axis;

   ---------------------------------------------------------------------------
   --  The term of a pair
   ---------------------------------------------------------------------------

   Square_Error : constant := 4.1 * 2.0**(-53);
   --  The relative error of the computed |U - π|²: the rounded difference, two products and a sum.

   Inverse_Error : constant := 5.2 * 2.0**(-53);
   --  That of its reciprocal.

   Weight_Error : constant := 7.3 * 2.0**(-53);
   --  That of the two weights (U - A)/|U - π|² and B/|U - π|², up to underflow.

   Inverse_Limit : constant := 2.0**200 + 2.0**160;
   --  No computed reciprocal of a squared distance is larger.

   Slope_Reach : constant := 2.0**401;
   --  No squared distance of a pole from [0, 1] is larger.

   procedure Lemma_Square_Error (Xh, Z, B, P1, P2, S : Real)
   with
     Ghost,
     Pre  =>
       abs (Xh - Z) <= Epsilon * abs Z + Eta
       and then abs Z <= 2.0 * Big
       and then Z * Z + B * B >= Smallest_Square
       and then abs (P1 - Xh * Xh) <= Epsilon * abs (Xh * Xh) + Eta
       and then abs (P2 - B * B) <= Epsilon * abs (B * B) + Eta
       and then abs (S - (P1 + P2)) <= Epsilon * abs (P1 + P2) + Eta,
     Post => abs (S - (Z * Z + B * B)) <= Square_Error * (Z * Z + B * B);
   --  Xh is the rounded U - A, P1 and P2 the rounded squares and S their rounded sum. The squared distance is
   --  far above every underflow.

   procedure Lemma_Square_Error (Xh, Z, B, P1, P2, S : Real) is
      D  : constant Real := Z * Z + B * B;
      E  : constant Real := Xh - Z;
      AZ : constant Real := abs Z;
   begin
      pragma Assert (Z * Z >= 0.0 and then B * B >= 0.0);
      pragma Assert (AZ * AZ = Z * Z);
      pragma Assert (Xh * Xh - Z * Z = E * (2.0 * Z + E));
      pragma Assert (abs (2.0 * Z + E) <= (2.0 + Epsilon) * AZ + Eta);
      Lemma_Product_Bound (E, 2.0 * Z + E, Epsilon * AZ + Eta, (2.0 + Epsilon) * AZ + Eta);
      pragma
        Assert
          ((Epsilon * AZ + Eta) * ((2.0 + Epsilon) * AZ + Eta)
           = Epsilon * (2.0 + Epsilon) * (AZ * AZ) + (2.0 + 2.0 * Epsilon) * Eta * AZ + Eta * Eta);
      pragma Assert ((2.0 + 2.0 * Epsilon) * Eta * AZ + Eta * Eta <= 0.01 * Epsilon * Smallest_Square);
      pragma Assert (abs (Xh * Xh - Z * Z) <= 2.01 * Epsilon * (Z * Z) + 0.01 * Epsilon * D);
      pragma Assert (abs (Xh * Xh) <= (1.0 + 2.01 * Epsilon) * (Z * Z) + 0.01 * Epsilon * D);
      pragma Assert (Eta <= 0.001 * Epsilon * D);
      pragma Assert (abs (P1 - Z * Z) <= 3.02 * Epsilon * (Z * Z) + 0.02 * Epsilon * D);
      pragma Assert (abs (P2 - B * B) <= Epsilon * (B * B) + 0.001 * Epsilon * D);
      pragma Assert (abs ((P1 + P2) - D) <= 3.05 * Epsilon * D);
      pragma Assert (abs (P1 + P2) <= (1.0 + 3.05 * Epsilon) * D);
      pragma Assert (abs (S - (P1 + P2)) <= 1.01 * Epsilon * D);
   end Lemma_Square_Error;

   procedure Lemma_Weight_Ranges (X, B, Sq : Dimensionless)
   with
     Ghost,
     Pre  =>
       abs X <= 2.0**201
       and then B > 0.0
       and then B <= Magnitude_Limit
       and then Sq >= 2.0**(-201)
       and then Sq <= 2.0**403,
     Post =>
       1.0 / Sq >= 0.0
       and then 1.0 / Sq <= 2.0**201
       and then abs (X * (1.0 / Sq)) <= 2.0**402
       and then B * (1.0 / Sq) >= 0.0
       and then B * (1.0 / Sq) <= 2.0**402;
   --  The reciprocal of a squared distance and the two weights are finite.

   procedure Lemma_Reciprocal_Range (Sq : Dimensionless)
   with Ghost, Pre => Sq >= 2.0**(-201) and then Sq <= 2.0**403, Post => 1.0 / Sq >= 0.0 and then 1.0 / Sq <= 2.0**201;

   procedure Lemma_Reciprocal_Range (Sq : Dimensionless) is null;

   procedure Lemma_Weight_Range (X, Inv : Dimensionless)
   with
     Ghost,
     Pre  => abs X <= 2.0**201 and then Inv >= 0.0 and then Inv <= 2.0**201,
     Post => abs (X * Inv) <= 2.0**402;

   procedure Lemma_Weight_Range (X, Inv : Dimensionless) is null;

   procedure Lemma_Positive_Weight_Range (B, Inv : Dimensionless)
   with
     Ghost,
     Pre  => B > 0.0 and then B <= Magnitude_Limit and then Inv >= 0.0 and then Inv <= 2.0**201,
     Post => B * Inv >= 0.0 and then B * Inv <= 2.0**402;

   procedure Lemma_Positive_Weight_Range (B, Inv : Dimensionless) is null;

   procedure Lemma_Weight_Ranges (X, B, Sq : Dimensionless) is
   begin
      Lemma_Reciprocal_Range (Sq);
      Lemma_Weight_Range (X, 1.0 / Sq);
      Lemma_Positive_Weight_Range (B, 1.0 / Sq);
   end Lemma_Weight_Ranges;

   procedure Lemma_Pair_Square (U, A, B, X : Dimensionless)
   with
     Ghost,
     Pre  =>
       U >= 0.0
       and then U <= 1.0
       and then abs A <= Magnitude_Limit
       and then B > 0.0
       and then B <= Magnitude_Limit
       and then
         Poles.Horizontal_Distance (R (A)) * Poles.Horizontal_Distance (R (A)) + R (B) * R (B) >= Smallest_Square
       and then abs (R (X) - (R (U) - R (A))) <= Epsilon * abs (R (U) - R (A)) + Eta,
     Post =>
       abs X <= 2.0**201
       and then abs (X * X) <= 2.0**403
       and then abs (B * B) <= 2.0**403
       and then X * X + B * B >= 2.0**(-201)
       and then X * X + B * B <= 2.0**403
       and then 1.0 / (X * X + B * B) >= 0.0
       and then 1.0 / (X * X + B * B) <= 2.0**201
       and then abs (X * (1.0 / (X * X + B * B))) <= 2.0**402
       and then B * (1.0 / (X * X + B * B)) >= 0.0
       and then B * (1.0 / (X * X + B * B)) <= 2.0**402
       and then abs (R (U) - R (A)) <= 2.0 * Big
       and then (R (U) - R (A)) * (R (U) - R (A)) + R (B) * R (B) >= Smallest_Square
       and then
         abs (R (X * X + B * B) - ((R (U) - R (A)) * (R (U) - R (A)) + R (B) * R (B)))
         <= Square_Error * ((R (U) - R (A)) * (R (U) - R (A)) + R (B) * R (B));
   --  X is the rounded difference of the computed distance and the real part of a pair. The computed squared
   --  distance from the pair is accurate, and no smaller than half the least which a prepared cache allows.

   procedure Lemma_Pair_Square (U, A, B, X : Dimensionless) is
      Z : constant Real := R (U) - R (A);
      D : constant Real := Z * Z + R (B) * R (B);
   begin
      Lemma_Limit (A);
      Lemma_Limit (B);
      Lemma_Horizontal_Reach (R (A), R (U));
      pragma Assert (D >= Smallest_Square);
      pragma Assert (abs Z <= 1.0 + Big);
      pragma Assert (abs (R (X)) <= (1.0 + Epsilon) * (1.0 + Big) + Eta);
      pragma Assert (abs X <= 2.0**201);
      Float_Lemmas.Lemma_Rounding_Error_Mul (X, X);
      Float_Lemmas.Lemma_Rounding_Error_Mul (B, B);
      pragma Assert (abs (X * X) <= 2.0**403);
      pragma Assert (abs (B * B) <= 2.0**403);
      Float_Lemmas.Lemma_Rounding_Error_Add (X * X, B * B);
      Lemma_Square_Error (R (X), Z, R (B), R (X * X), R (B * B), R (X * X + B * B));
      Lemma_Product_Bound (Z, Z, 2.0 * Big, 2.0 * Big);
      Lemma_Product_Bound (R (B), R (B), Big, Big);
      pragma Assert (D <= 5.0 * (Big * Big));
      pragma Assert (R (X * X + B * B) <= (1.0 + Square_Error) * (5.0 * (Big * Big)));
      pragma Assert (R (X * X + B * B) >= (1.0 - Square_Error) * Smallest_Square);
      pragma Assert (X * X + B * B >= 2.0**(-201));
      pragma Assert (X * X + B * B <= 2.0**403);
      Lemma_Weight_Ranges (X, B, X * X + B * B);
   end Lemma_Pair_Square;

   procedure Lemma_Pair_Square_Term (U : Dimensionless; Term : Pole_Term; X : Dimensionless)
   with
     Ghost,
     Pre  =>
       U >= 0.0
       and then U <= 1.0
       and then Is_Term_Ready (Term)
       and then Term.Imaginary_Part /= 0.0
       and then abs (R (X) - (R (U) - R (Term.Real_Part))) <= Epsilon * abs (R (U) - R (Term.Real_Part)) + Eta,
     Post =>
       Term.Imaginary_Part > 0.0
       and then Term.Imaginary_Part <= Magnitude_Limit
       and then abs X <= 2.0**201
       and then abs (X * X) <= 2.0**403
       and then abs (Term.Imaginary_Part * Term.Imaginary_Part) <= 2.0**403
       and then X * X + Term.Imaginary_Part * Term.Imaginary_Part >= 2.0**(-201)
       and then X * X + Term.Imaginary_Part * Term.Imaginary_Part <= 2.0**403
       and then 1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part) >= 0.0
       and then 1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part) <= 2.0**201
       and then abs (X * (1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part))) <= 2.0**402
       and then Term.Imaginary_Part * (1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part)) >= 0.0
       and then Term.Imaginary_Part * (1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part)) <= 2.0**402
       and then
         (R (U) - R (Term.Real_Part)) * (R (U) - R (Term.Real_Part))
         + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)
         >= Smallest_Square
       and then
         abs (R (X * X + Term.Imaginary_Part * Term.Imaginary_Part)
              - ((R (U) - R (Term.Real_Part)) * (R (U) - R (Term.Real_Part))
                 + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
         <= Square_Error
            * ((R (U) - R (Term.Real_Part)) * (R (U) - R (Term.Real_Part))
               + R (Term.Imaginary_Part) * R (Term.Imaginary_Part));
   --  The same, for a gathered pair.

   procedure Lemma_Pair_Square_Term (U : Dimensionless; Term : Pole_Term; X : Dimensionless) is
      H : constant Real := Poles.Horizontal_Distance (R (Term.Real_Part));
   begin
      pragma Assert (Poles.Is_Term_Bounded (Term));
      pragma Assert (abs Term.Real_Part <= Magnitude_Limit);
      pragma Assert (Term.Imaginary_Part > 0.0 and then Term.Imaginary_Part <= Magnitude_Limit);
      pragma Assert (Poles.Distance_Square (Term) >= Smallest_Square);
      pragma Assert (Poles.Distance_Square (Term) = H * H + R (Term.Imaginary_Part) * R (Term.Imaginary_Part));
      Lemma_Pair_Square (U, Term.Real_Part, Term.Imaginary_Part, X);
   end Lemma_Pair_Square_Term;

   function Is_Pair_Weight (Z, B, Inv, Wr, Wi : Real) return Boolean
   with
     Ghost,
     Post =>
       Is_Pair_Weight'Result
       = (B > 0.0
          and then Z * Z + B * B >= Smallest_Square
          and then abs (Inv - 1.0 / (Z * Z + B * B)) <= Inverse_Error * (1.0 / (Z * Z + B * B))
          and then
            abs (Wr - Z * (1.0 / (Z * Z + B * B)))
            <= Weight_Error * (abs Z * (1.0 / (Z * Z + B * B))) + 1.01 * Eta * (1.0 / (Z * Z + B * B)) + Eta
          and then abs (Wi - B * (1.0 / (Z * Z + B * B))) <= Weight_Error * (B * (1.0 / (Z * Z + B * B))) + Eta);
   --  With Z = U - A at the computed distance: Inv is the computed 1/|U - π|², and Wr and Wi are the two
   --  computed weights. An underflow in the difference reaches Wr amplified by the reciprocal.

   function Is_Pair_Weight (Z, B, Inv, Wr, Wi : Real) return Boolean is
   begin
      return
        B > 0.0
        and then Z * Z + B * B >= Smallest_Square
        and then abs (Inv - 1.0 / (Z * Z + B * B)) <= Inverse_Error * (1.0 / (Z * Z + B * B))
        and then
          abs (Wr - Z * (1.0 / (Z * Z + B * B)))
          <= Weight_Error * (abs Z * (1.0 / (Z * Z + B * B))) + 1.01 * Eta * (1.0 / (Z * Z + B * B)) + Eta
        and then abs (Wi - B * (1.0 / (Z * Z + B * B))) <= Weight_Error * (B * (1.0 / (Z * Z + B * B))) + Eta;
   end Is_Pair_Weight;

   procedure Lemma_Pair_Weight_Intro (Z, B, Inv, Wr, Wi : Real)
   with
     Ghost,
     Pre  =>
       B > 0.0
       and then Z * Z + B * B >= Smallest_Square
       and then abs (Inv - 1.0 / (Z * Z + B * B)) <= Inverse_Error * (1.0 / (Z * Z + B * B))
       and then
         abs (Wr - Z * (1.0 / (Z * Z + B * B)))
         <= Weight_Error * (abs Z * (1.0 / (Z * Z + B * B))) + 1.01 * Eta * (1.0 / (Z * Z + B * B)) + Eta
       and then abs (Wi - B * (1.0 / (Z * Z + B * B))) <= Weight_Error * (B * (1.0 / (Z * Z + B * B))) + Eta,
     Post => Is_Pair_Weight (Z, B, Inv, Wr, Wi);

   procedure Lemma_Pair_Weight_Intro (Z, B, Inv, Wr, Wi : Real) is null;

   procedure Lemma_Pair_Weight_Elim (Z, B, Inv, Wr, Wi : Real)
   with
     Ghost,
     Pre  => Is_Pair_Weight (Z, B, Inv, Wr, Wi),
     Post =>
       B > 0.0
       and then Z * Z + B * B >= Smallest_Square
       and then abs (Inv - 1.0 / (Z * Z + B * B)) <= Inverse_Error * (1.0 / (Z * Z + B * B))
       and then
         abs (Wr - Z * (1.0 / (Z * Z + B * B)))
         <= Weight_Error * (abs Z * (1.0 / (Z * Z + B * B))) + 1.01 * Eta * (1.0 / (Z * Z + B * B)) + Eta
       and then abs (Wi - B * (1.0 / (Z * Z + B * B))) <= Weight_Error * (B * (1.0 / (Z * Z + B * B))) + Eta;

   procedure Lemma_Pair_Weight_Elim (Z, B, Inv, Wr, Wi : Real) is null;

   procedure Lemma_Weight_Real (Xh, Z, Iv, RD, W : Real)
   with
     Ghost,
     Pre  =>
       abs (Xh - Z) <= Epsilon * abs Z + Eta
       and then RD > 0.0
       and then abs (Iv - RD) <= Inverse_Error * RD
       and then abs (W - Xh * Iv) <= Epsilon * abs (Xh * Iv) + Eta,
     Post => abs (W - Z * RD) <= Weight_Error * (abs Z * RD) + 1.01 * Eta * RD + Eta;

   procedure Lemma_Weight_Real (Xh, Z, Iv, RD, W : Real) is
      pragma Unreferenced (W);
      ZR : constant Real := abs Z * RD;
   begin
      pragma Assert (Xh * Iv - Z * RD = (Xh - Z) * Iv + Z * (Iv - RD));
      pragma Assert (abs Iv <= (1.0 + Inverse_Error) * RD);
      Lemma_Product_Bound (Xh - Z, Iv, Epsilon * abs Z + Eta, (1.0 + Inverse_Error) * RD);
      pragma
        Assert
          ((Epsilon * abs Z + Eta) * ((1.0 + Inverse_Error) * RD)
           = Epsilon * (1.0 + Inverse_Error) * ZR + (1.0 + Inverse_Error) * Eta * RD);
      Lemma_Product_Bound (Z, Iv - RD, abs Z, Inverse_Error * RD);
      pragma Assert (abs Z * (Inverse_Error * RD) = Inverse_Error * ZR);
      pragma
        Assert
          (abs (Xh * Iv - Z * RD)
           <= (Epsilon * (1.0 + Inverse_Error) + Inverse_Error) * ZR + (1.0 + Inverse_Error) * Eta * RD);
      pragma Assert (abs (Z * RD) = ZR);
      pragma
        Assert
          (abs (Xh * Iv)
           <= ZR + (Epsilon * (1.0 + Inverse_Error) + Inverse_Error) * ZR + (1.0 + Inverse_Error) * Eta * RD);
      pragma Assert (ZR >= 0.0);
   end Lemma_Weight_Real;

   procedure Lemma_Weight_Imaginary (B, Iv, RD, W : Real)
   with
     Ghost,
     Pre  =>
       B >= 0.0
       and then RD > 0.0
       and then abs (Iv - RD) <= Inverse_Error * RD
       and then abs (W - B * Iv) <= Epsilon * abs (B * Iv) + Eta,
     Post => abs (W - B * RD) <= Weight_Error * (B * RD) + Eta;

   procedure Lemma_Weight_Imaginary (B, Iv, RD, W : Real) is
      pragma Unreferenced (W);
      BR : constant Real := B * RD;
   begin
      Lemma_Product_Bound (B, Iv - RD, B, Inverse_Error * RD);
      pragma Assert (B * Iv - BR = B * (Iv - RD));
      pragma Assert (B * (Inverse_Error * RD) = Inverse_Error * BR);
      pragma Assert (abs (B * Iv - BR) <= Inverse_Error * BR);
      pragma Assert (BR >= 0.0);
      pragma Assert (abs (B * Iv) <= (1.0 + Inverse_Error) * BR);
   end Lemma_Weight_Imaginary;

   procedure Lemma_Pair_Weights (X, B, Sq, Inv, Wr, Wi : Dimensionless; Z : Real)
   with
     Ghost,
     Pre  =>
       abs X <= 2.0**201
       and then B > 0.0
       and then B <= Magnitude_Limit
       and then Sq >= 2.0**(-201)
       and then Sq <= 2.0**403
       and then abs Inv <= 2.0**202
       and then abs (R (X) - Z) <= Epsilon * abs Z + Eta
       and then Z * Z + R (B) * R (B) >= Smallest_Square
       and then abs (R (Sq) - (Z * Z + R (B) * R (B))) <= Square_Error * (Z * Z + R (B) * R (B))
       and then R (Inv) = R (1.0 / Sq)
       and then R (Wr) = R (X * Inv)
       and then R (Wi) = R (B * Inv),
     Post =>
       Is_Pair_Weight (Z, R (B), R (Inv), R (Wr), R (Wi))
       and then abs (R (Inv) - 1.0 / (Z * Z + R (B) * R (B))) <= Inverse_Error * (1.0 / (Z * Z + R (B) * R (B)))
       and then R (Inv) > 0.0
       and then R (Inv) <= Inverse_Limit;
   --  The reciprocal of the squared distance and the two weights, as the node loop computes them.

   procedure Lemma_Pair_Weights (X, B, Sq, Inv, Wr, Wi : Dimensionless; Z : Real) is
      D  : constant Real := Z * Z + R (B) * R (B);
      RD : constant Real := 1.0 / D;
   begin
      Lemma_Limit (B);
      pragma Assert (R (B) > 0.0);
      Lemma_Reciprocal (D);
      pragma Assert (RD > 0.0);
      Lemma_Reciprocal_Ceiling (D, Smallest_Square);
      pragma Assert (RD * Smallest_Square <= 1.0);
      pragma Assert (RD <= Big);
      --  The reciprocal.
      pragma Assert (R (Sq) >= (1.0 - Square_Error) * D);
      pragma Assert (R (Sq) > 0.0);
      Float_Lemmas.Lemma_Rounding_Error_Div (1.0, Sq);
      pragma Assert (R (1.0) = 1.0);
      Lemma_Reciprocal_Floor (R (Sq), 2.0 * (Big * Big) * 8.0);
      pragma Assert (abs (1.0 / R (Sq)) >= Normal_Floor);
      Lemma_Normal_Rounding (R (1.0 / Sq), 1.0 / R (Sq));
      Lemma_Relative_Reciprocal (R (Sq), D, Square_Error, 4.11 * Epsilon);
      Lemma_Relative_Chain (R (1.0 / Sq), 1.0 / R (Sq), RD, Unit, 4.11 * Epsilon, Inverse_Error);
      pragma Assert (abs (R (Inv) - RD) <= Inverse_Error * RD);
      pragma Assert (R (Inv) >= (1.0 - Inverse_Error) * RD);
      pragma Assert (R (Inv) > 0.0);
      pragma Assert (R (Inv) <= (1.0 + Inverse_Error) * Big);
      pragma Assert (R (Inv) <= Inverse_Limit);
      --  The weights.
      Float_Lemmas.Lemma_Rounding_Error_Mul (X, Inv);
      Lemma_Weight_Real (R (X), Z, R (Inv), RD, R (Wr));
      Float_Lemmas.Lemma_Rounding_Error_Mul (B, Inv);
      Lemma_Weight_Imaginary (R (B), R (Inv), RD, R (Wi));
      Lemma_Pair_Weight_Intro (Z, R (B), R (Inv), R (Wr), R (Wi));
   end Lemma_Pair_Weights;

   procedure Lemma_Pair_Rounding (A1, A2, T, P1, P2 : Real)
   with
     Ghost,
     Pre  =>
       abs (A1 - P1) <= Epsilon * abs P1 + Eta
       and then abs (A2 - P2) <= Epsilon * abs P2 + Eta
       and then abs (T - (A1 - A2)) <= Epsilon * abs (A1 - A2) + Eta,
     Post =>
       abs (T - (P1 - P2)) <= 2.01 * Epsilon * (abs P1 + abs P2) + 3.01 * Eta
       and then abs T <= (1.0 + 2.1 * Epsilon) * (abs P1 + abs P2) + Term_Dust;
   --  Two rounded products and their rounded difference.

   procedure Lemma_Pair_Rounding (A1, A2, T, P1, P2 : Real) is
      pragma Unreferenced (T);
   begin
      pragma Assert (abs (A1 - A2) <= (1.0 + Epsilon) * (abs P1 + abs P2) + 2.0 * Eta);
      pragma Assert (abs ((A1 - A2) - (P1 - P2)) <= Epsilon * (abs P1 + abs P2) + 2.0 * Eta);
   end Lemma_Pair_Rounding;

   procedure Lemma_Pair_Size (S1, S2, S, M1, M2 : Real)
   with
     Ghost,
     Pre  =>
       M1 >= 0.0
       and then M2 >= 0.0
       and then S1 >= 0.0
       and then S2 >= 0.0
       and then abs (S1 - M1) <= Epsilon * M1 + Eta
       and then abs (S2 - M2) <= Epsilon * M2 + Eta
       and then abs (S - (S1 + S2)) <= Epsilon * abs (S1 + S2) + Eta,
     Post =>
       S >= (1.0 - 2.1 * Epsilon) * (M1 + M2) - Term_Dust and then S <= (1.0 + 2.1 * Epsilon) * (M1 + M2) + Term_Dust;
   --  Two rounded magnitudes and their rounded sum.

   procedure Lemma_Pair_Size (S1, S2, S, M1, M2 : Real) is
      pragma Unreferenced (S);
   begin
      pragma Assert (S1 + S2 >= (1.0 - Epsilon) * (M1 + M2) - 2.0 * Eta);
      pragma Assert (S1 + S2 <= (1.0 + Epsilon) * (M1 + M2) + 2.0 * Eta);
   end Lemma_Pair_Size;

   procedure Lemma_Weighted_Error (Residue, W, Target, Slip : Real)
   with
     Ghost,
     Pre  => Slip >= 0.0 and then abs (W - Target) <= Weight_Error * abs Target + Slip,
     Post =>
       abs (Residue * W - Residue * Target) <= 7.31 * Epsilon * (abs Residue * abs W) + 1.01 * (abs Residue * Slip);
   --  The error of a weight, against the computed weight instead of the exact one, in a term.

   procedure Lemma_Weighted_Error (Residue, W, Target, Slip : Real) is
      Bound : constant Real := 7.31 * Epsilon * abs W + 1.01 * Slip;
   begin
      pragma Assert ((1.0 - Weight_Error) * abs Target <= abs W + Slip);
      pragma Assert (Weight_Error * abs Target <= 7.31 * Epsilon * (abs W + Slip));
      pragma Assert (abs (W - Target) <= Bound);
      pragma Assert (Residue * W - Residue * Target = Residue * (W - Target));
      Lemma_Product_Bound (Residue, W - Target, abs Residue, Bound);
      pragma Assert (abs Residue * Bound = 7.31 * Epsilon * (abs Residue * abs W) + 1.01 * (abs Residue * Slip));
   end Lemma_Weighted_Error;

   procedure Lemma_Pair_Term_Error (Rr, Ri, Wr, Wi, Z, B, RD, Sl, T : Real)
   with
     Ghost,
     Pre  =>
       RD > 0.0
       and then B > 0.0
       and then Wi >= 0.0
       and then abs (Wr - Z * RD) <= Weight_Error * (abs Z * RD) + 1.01 * Eta * RD + Eta
       and then abs (Wi - B * RD) <= Weight_Error * (B * RD) + Eta
       and then abs (T - (Rr * Wr - Ri * Wi)) <= 2.01 * Epsilon * (abs Rr * abs Wr + abs Ri * Wi) + 3.01 * Eta
       and then Sl >= 0.0
       and then abs Rr * RD <= Sl
       and then abs Rr + abs Ri <= Slope_Reach * Sl,
     Post =>
       abs (T - (Rr * (Z * RD) - Ri * (B * RD)))
       <= Term_Error * (abs Rr * abs Wr + abs Ri * Wi) + Zeta * Sl + Term_Dust;
   --  The rounded term of a pair against the exact one: the rounding of its own operations, the errors of
   --  the two weights, and the underflows which the residues amplify, which a sliver of the slope covers.

   procedure Lemma_Pair_Term_Error (Rr, Ri, Wr, Wi, Z, B, RD, Sl, T : Real) is
      M1   : constant Real := abs Rr * abs Wr;
      M2   : constant Real := abs Ri * Wi;
      Tr   : constant Real := Z * RD;
      Ti   : constant Real := B * RD;
      Slip : constant Real := 1.01 * Eta * RD + Eta;
   begin
      pragma Assert (abs Wi = Wi);
      pragma Assert (abs Tr = abs Z * RD);
      Lemma_Scale (0.0, B, RD);
      pragma Assert (abs Ti = B * RD);
      pragma Assert (abs (Wr - Tr) <= Weight_Error * abs Tr + Slip);
      pragma Assert (abs (Wi - Ti) <= Weight_Error * abs Ti + Eta);
      Lemma_Weighted_Error (Rr, Wr, Tr, Slip);
      Lemma_Weighted_Error (Ri, Wi, Ti, Eta);
      pragma Assert (abs Rr * Slip = 1.01 * Eta * (abs Rr * RD) + Eta * abs Rr);
      pragma Assert (abs Rr * Slip <= 1.01 * Eta * Sl + Eta * abs Rr);
      pragma
        Assert (1.01 * (abs Rr * Slip) + 1.01 * (abs Ri * Eta) <= 1.03 * Eta * Sl + 1.01 * Eta * (abs Rr + abs Ri));
      pragma Assert (1.01 * Eta * (abs Rr + abs Ri) <= 1.01 * Eta * (Slope_Reach * Sl));
      pragma Assert (1.03 * Eta * Sl + 1.01 * Eta * (Slope_Reach * Sl) = (1.03 * Eta + 1.01 * Eta * Slope_Reach) * Sl);
      Lemma_Scale (1.03 * Eta + 1.01 * Eta * Slope_Reach, Zeta, Sl);
      pragma Assert (1.01 * (abs Rr * Slip) + 1.01 * (abs Ri * Eta) <= Zeta * Sl);
      pragma
        Assert
          (abs (T - (Rr * Tr - Ri * Ti))
           <= abs (T - (Rr * Wr - Ri * Wi)) + abs (Rr * Wr - Rr * Tr) + abs (Ri * Wi - Ri * Ti));
      pragma Assert (abs (Rr * Wr - Rr * Tr) + abs (Ri * Wi - Ri * Ti) <= 7.31 * Epsilon * (M1 + M2) + Zeta * Sl);
   end Lemma_Pair_Term_Error;

   procedure Lemma_Pair_Products (Rr, Ri, Wr, Wi : Dimensionless)
   with
     Ghost,
     Pre  =>
       abs Rr <= Magnitude_Limit
       and then abs Ri <= Magnitude_Limit
       and then abs Wr <= 2.0**402
       and then Wi >= 0.0
       and then Wi <= 2.0**402,
     Post =>
       abs (Rr * Wr) <= 2.0**603
       and then abs (Ri * Wi) <= 2.0**603
       and then abs Rr * abs Wr >= 0.0
       and then abs Rr * abs Wr <= 2.0**603
       and then abs Ri * Wi >= 0.0
       and then abs Ri * Wi <= 2.0**603
       and then abs Rr * abs Wr + abs Ri * Wi >= 0.0
       and then R (Wi) >= 0.0
       and then abs (R (Rr)) * abs (R (Wr)) + abs (R (Ri)) * R (Wi) >= 0.0
       and then abs (R (Rr * Wr - Ri * Wi)) <= Half_Term_Limit
       and then R (abs Rr * abs Wr + abs Ri * Wi) <= Half_Term_Limit
       and then
         abs (R (Rr * Wr - Ri * Wi) - (R (Rr) * R (Wr) - R (Ri) * R (Wi)))
         <= 2.01 * Epsilon * (abs (R (Rr)) * abs (R (Wr)) + abs (R (Ri)) * R (Wi)) + 3.01 * Eta
       and then
         abs (R (Rr * Wr - Ri * Wi))
         <= (1.0 + 2.1 * Epsilon) * (abs (R (Rr)) * abs (R (Wr)) + abs (R (Ri)) * R (Wi)) + Term_Dust
       and then
         R (abs Rr * abs Wr + abs Ri * Wi)
         >= (1.0 - 2.1 * Epsilon) * (abs (R (Rr)) * abs (R (Wr)) + abs (R (Ri)) * R (Wi)) - Term_Dust;
   --  The rounded operations of the term of a pair and of its magnitude: nothing overflows, and their results
   --  are those of the exact operations on the same weights, to within their rounding.

   procedure Lemma_Pair_Products (Rr, Ri, Wr, Wi : Dimensionless) is
      Vr    : constant Real := R (Rr);
      Vi    : constant Real := R (Ri);
      M1    : constant Real := abs Vr * abs (R (Wr));
      M2    : constant Real := abs Vi * R (Wi);
      A1    : constant Dimensionless := Rr * Wr;
      A2    : constant Dimensionless := Ri * Wi;
      S1    : constant Dimensionless := abs Rr * abs Wr;
      S2    : constant Dimensionless := abs Ri * Wi;
      Limit : constant Real := 4.0 * (Big * Big * Big);
   begin
      Lemma_Limit (Rr);
      Lemma_Limit (Ri);
      pragma Assert (R (Wi) >= 0.0);
      pragma Assert (abs (R (Wr)) <= 4.0 * (Big * Big));
      pragma Assert (R (Wi) <= 4.0 * (Big * Big));
      Lemma_Product_Bound (Vr, R (Wr), Big, 4.0 * (Big * Big));
      Lemma_Product_Bound (Vi, R (Wi), Big, 4.0 * (Big * Big));
      pragma Assert (abs (Vr * R (Wr)) = M1);
      pragma Assert (abs (Vi * R (Wi)) = M2);
      pragma Assert (M1 >= 0.0 and then M1 <= Limit);
      pragma Assert (M2 >= 0.0 and then M2 <= Limit);
      pragma Assert (8.0 * Limit <= Half_Term_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Mul (Rr, Wr);
      Float_Lemmas.Lemma_Rounding_Error_Mul (Ri, Wi);
      pragma Assert (abs (R (A1)) <= 2.0 * Limit);
      pragma Assert (abs (R (A2)) <= 2.0 * Limit);
      pragma Assert (abs A1 <= 2.0**603);
      pragma Assert (abs A2 <= 2.0**603);
      Float_Lemmas.Lemma_Rounding_Error_Sub (A1, A2);
      Lemma_Pair_Rounding (R (A1), R (A2), R (A1 - A2), Vr * R (Wr), Vi * R (Wi));
      Lemma_Abs (Rr);
      Lemma_Abs (Wr);
      Lemma_Abs (Ri);
      Float_Lemmas.Lemma_Rounding_Error_Mul (abs Rr, abs Wr);
      Float_Lemmas.Lemma_Rounding_Error_Mul (abs Ri, Wi);
      pragma Assert (R (abs Rr) * R (abs Wr) = M1);
      pragma Assert (abs (R (abs Rr) * R (abs Wr)) = M1);
      pragma Assert (abs (R (S1) - M1) <= Epsilon * M1 + Eta);
      pragma Assert (R (abs Ri) * R (Wi) = M2);
      pragma Assert (abs (R (abs Ri) * R (Wi)) = M2);
      pragma Assert (abs (R (S2) - M2) <= Epsilon * M2 + Eta);
      pragma Assert (S1 >= 0.0 and then S2 >= 0.0);
      pragma Assert (S1 <= 2.0**603 and then S2 <= 2.0**603);
      Float_Lemmas.Lemma_Rounding_Error_Add (S1, S2);
      Lemma_Pair_Size (R (S1), R (S2), R (S1 + S2), M1, M2);
      pragma Assert (S1 + S2 >= 0.0);
   end Lemma_Pair_Products;

   procedure Lemma_Pair_Term (Rr, Ri, Wr, Wi : Dimensionless; Z, B, Inv, Sl : Real)
   with
     Ghost,
     Pre  =>
       abs Rr <= Magnitude_Limit
       and then abs Ri <= Magnitude_Limit
       and then abs Wr <= 2.0**402
       and then Wi >= 0.0
       and then Wi <= 2.0**402
       and then Is_Pair_Weight (Z, B, Inv, R (Wr), R (Wi))
       and then Z * Z + B * B > 0.0
       and then Sl >= 0.0
       and then abs (R (Rr)) * (1.0 / (Z * Z + B * B)) <= Sl
       and then abs (R (Rr)) + abs (R (Ri)) <= Slope_Reach * Sl,
     Post =>
       abs (Rr * Wr) <= 2.0**603
       and then abs (Ri * Wi) <= 2.0**603
       and then abs Rr * abs Wr >= 0.0
       and then abs Rr * abs Wr <= 2.0**603
       and then abs Ri * Wi >= 0.0
       and then abs Ri * Wi <= 2.0**603
       and then abs Rr * abs Wr + abs Ri * Wi >= 0.0
       and then abs (R (Rr)) * abs (R (Wr)) + abs (R (Ri)) * R (Wi) >= 0.0
       and then abs (R (Rr * Wr - Ri * Wi)) <= Half_Term_Limit
       and then R (abs Rr * abs Wr + abs Ri * Wi) <= Half_Term_Limit
       and then
         abs (R (Rr * Wr - Ri * Wi) - (R (Rr) * Z - R (Ri) * B) / (Z * Z + B * B))
         <= Term_Error * (abs (R (Rr)) * abs (R (Wr)) + abs (R (Ri)) * R (Wi)) + Zeta * Sl + Term_Dust
       and then
         abs (R (Rr * Wr - Ri * Wi))
         <= (1.0 + 2.1 * Epsilon) * (abs (R (Rr)) * abs (R (Wr)) + abs (R (Ri)) * R (Wi)) + Term_Dust
       and then
         R (abs Rr * abs Wr + abs Ri * Wi)
         >= (1.0 - 2.1 * Epsilon) * (abs (R (Rr)) * abs (R (Wr)) + abs (R (Ri)) * R (Wi)) - Term_Dust;
   --  The rounded term of a pair and its rounded magnitude.

   procedure Lemma_Pair_Term (Rr, Ri, Wr, Wi : Dimensionless; Z, B, Inv, Sl : Real) is
      D  : constant Real := Z * Z + B * B;
      RD : constant Real := 1.0 / D;
      Vr : constant Real := R (Rr);
      Vi : constant Real := R (Ri);
   begin
      Lemma_Pair_Weight_Elim (Z, B, Inv, R (Wr), R (Wi));
      Lemma_Reciprocal (D);
      pragma Assert (RD > 0.0);
      Lemma_Pair_Products (Rr, Ri, Wr, Wi);
      Lemma_Pair_Term_Error (Vr, Vi, R (Wr), R (Wi), Z, B, RD, Sl, R (Rr * Wr - Ri * Wi));
      Lemma_Division (Vr * Z - Vi * B, D);
      pragma Assert ((Vr * Z - Vi * B) * RD = Vr * (Z * RD) - Vi * (B * RD));
   end Lemma_Pair_Term;

   procedure Lemma_Pair_Axis
     (Prepared : Preparation;
      Index    : Rational_Degree_Slot;
      Axis     : Axis_Name;
      U        : Dimensionless;
      Wr, Wi   : Dimensionless;
      Ret      : Dimensionless;
      Size     : Dimensionless;
      Inv, M   : Real)
   with
     Ghost,
     Pre  =>
       U >= 0.0
       and then U <= 1.0
       and then Is_Term_Ready (Prepared.Terms (Index))
       and then Prepared.Terms (Index).Imaginary_Part > 0.0
       and then abs Wr <= 2.0**402
       and then Wi >= 0.0
       and then Wi <= 2.0**402
       and then
         Is_Pair_Weight
           (R (U) - R (Prepared.Terms (Index).Real_Part),
            R (Prepared.Terms (Index).Imaginary_Part),
            Inv,
            R (Wr),
            R (Wi))
       and then Is_Axis_State (Prepared, Index - 1, Axis, U, Ret, Size, M),
     Post =>
       abs (Prepared.Terms (Index).Residue_Real (Axis) * Wr) <= 2.0**603
       and then abs (Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi) <= 2.0**603
       and then abs Prepared.Terms (Index).Residue_Real (Axis) * abs Wr <= 2.0**603
       and then abs Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi <= 2.0**603
       and then abs Ret <= 2.0**610
       and then abs Size <= 2.0**610
       and then
         Is_Axis_State
           (Prepared,
            Index,
            Axis,
            U,
            Ret
            + (Prepared.Terms (Index).Residue_Real (Axis) * Wr - Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi),
            Size
            + (abs Prepared.Terms (Index).Residue_Real (Axis) * abs Wr
               + abs Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi),
            M
            + (abs (R (Prepared.Terms (Index).Residue_Real (Axis))) * abs (R (Wr))
               + abs (R (Prepared.Terms (Index).Residue_Imaginary (Axis))) * R (Wi)));
   --  One pair more on one axis.

   procedure Lemma_Pair_Slope (Term : Pole_Term; Axis : Axis_Name; V : Real)
   with
     Ghost,
     Pre  => Poles.Is_Term_Bounded (Term) and then Term.Imaginary_Part > 0.0 and then V >= 0.0 and then V <= 1.0,
     Post =>
       Poles.Slope_Term (Term, Axis) >= 0.0
       and then
         (V - R (Term.Real_Part)) * (V - R (Term.Real_Part)) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)
         >= Smallest_Square
       and then
         abs (R (Term.Residue_Real (Axis)))
         * (1.0
            / ((V - R (Term.Real_Part)) * (V - R (Term.Real_Part))
               + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
         <= Poles.Slope_Term (Term, Axis)
       and then
         abs (R (Term.Residue_Real (Axis))) + abs (R (Term.Residue_Imaginary (Axis)))
         <= Slope_Reach * Poles.Slope_Term (Term, Axis);
   --  The residues of a pair against its share of the slope: |R|/|V - π|² is no more than it, and |R| + |I| no
   --  more than the largest squared distance times it.

   procedure Lemma_Pair_Slope (Term : Pole_Term; Axis : Axis_Name; V : Real) is
      A      : constant Real := R (Term.Real_Part);
      B      : constant Real := R (Term.Imaginary_Part);
      Z      : constant Real := V - A;
      D      : constant Real := Z * Z + B * B;
      RD     : constant Real := 1.0 / D;
      H      : constant Real := Poles.Horizontal_Distance (A);
      Square : constant Real := Poles.Distance_Square (Term);
      Sl     : constant Real := Poles.Slope_Term (Term, Axis);
      Rr     : constant Real := abs (R (Term.Residue_Real (Axis)));
      Ri     : constant Real := abs (R (Term.Residue_Imaginary (Axis)));
   begin
      Lemma_Slope_Term_Sign (Term, Axis);
      pragma Assert (Sl * Square = Rr + Ri);
      Lemma_Limit (Term.Real_Part);
      Lemma_Limit (Term.Imaginary_Part);
      Lemma_Horizontal_Reach (A, V);
      pragma Assert (Square = H * H + B * B);
      pragma Assert (Square >= Smallest_Square);
      pragma Assert (D >= Square);
      pragma Assert (H <= Big);
      Lemma_Product_Bound (H, H, Big, Big);
      Lemma_Product_Bound (B, B, Big, Big);
      pragma Assert (Square <= Slope_Reach);
      Lemma_Scale (Square, Slope_Reach, Sl);
      pragma Assert (Rr + Ri <= Slope_Reach * Sl);
      Lemma_Reciprocal (D);
      pragma Assert (RD > 0.0);
      Lemma_Scale (Square, D, Sl);
      pragma Assert (Rr <= Sl * D);
      Lemma_Scale (Rr, Sl * D, RD);
      pragma Assert ((Sl * D) * RD = Sl * (RD * D));
      pragma Assert (Rr * RD <= Sl);
   end Lemma_Pair_Slope;

   procedure Lemma_Pair_Value (Term : Pole_Term; Axis : Axis_Name; V : Real)
   with
     Ghost,
     Pre  => Term.Imaginary_Part > 0.0,
     Post =>
       (V - R (Term.Real_Part)) * (V - R (Term.Real_Part)) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part) > 0.0
       and then
         Term_Value (Term, Axis, V)
         = (R (Term.Residue_Real (Axis)) * (V - R (Term.Real_Part))
            - R (Term.Residue_Imaginary (Axis)) * R (Term.Imaginary_Part))
           / ((V - R (Term.Real_Part)) * (V - R (Term.Real_Part)) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part));

   procedure Lemma_Pair_Value (Term : Pole_Term; Axis : Axis_Name; V : Real) is
      pragma Unreferenced (Axis);
      B : constant Real := R (Term.Imaginary_Part);
      Z : constant Real := V - R (Term.Real_Part);
   begin
      pragma Assert (B > 0.0);
      pragma Assert (B * B > 0.0);
      pragma Assert (Z * Z >= 0.0);
      pragma Assert (Z * Z + B * B > 0.0);
   end Lemma_Pair_Value;

   procedure Lemma_Pair_Axis
     (Prepared : Preparation;
      Index    : Rational_Degree_Slot;
      Axis     : Axis_Name;
      U        : Dimensionless;
      Wr, Wi   : Dimensionless;
      Ret      : Dimensionless;
      Size     : Dimensionless;
      Inv, M   : Real)
   is
      Term : constant Pole_Term := Prepared.Terms (Index);
      Rr   : constant Dimensionless := Term.Residue_Real (Axis);
      Ri   : constant Dimensionless := Term.Residue_Imaginary (Axis);
      A    : constant Real := R (Term.Real_Part);
      B    : constant Real := R (Term.Imaginary_Part);
      Z    : constant Real := R (U) - A;
      D    : constant Real := Z * Z + B * B;
      Mu   : constant Real := abs (R (Rr)) * abs (R (Wr)) + abs (R (Ri)) * R (Wi);
      Sl   : constant Real := Poles.Slope_Term (Term, Axis);
   begin
      pragma Assert (Poles.Is_Term_Bounded (Term));
      pragma Assert (abs Rr <= Magnitude_Limit);
      pragma Assert (abs Ri <= Magnitude_Limit);
      pragma Assert (R (U) >= 0.0 and then R (U) <= 1.0);
      Lemma_Pair_Slope (Term, Axis, R (U));
      Lemma_Pair_Value (Term, Axis, R (U));
      Lemma_Pair_Term (Rr, Ri, Wr, Wi, Z, B, Inv, Sl);
      pragma Assert (Mu >= 0.0);
      pragma Assert (Term_Value (Term, Axis, R (U)) = (R (Rr) * Z - R (Ri) * B) / D);
      Lemma_Axis_Step
        (Prepared,
         Index,
         Axis,
         U,
         Ret,
         Rr * Wr - Ri * Wi,
         Size,
         abs Rr * abs Wr + abs Ri * Wi,
         M,
         Mu,
         (R (Rr) * Z - R (Ri) * B) / D);
   end Lemma_Pair_Axis;

   ---------------------------------------------------------------------------
   --  The doubled residues of a pair
   ---------------------------------------------------------------------------

   --  A gathered pair holds its two residues doubled, and the doubling is only known to be as accurate as one
   --  rounded operation: within 2u of the stored residue, and η. So the term of a gathered pair is not quite the
   --  term of the pair in the cache. At a point V of the curve the two differ by at most Unit times the weight
   --  (|R|·|V - A| + |I|·B)/|V - π|² of the gathered term, and Gap_Dust. A weight moves no faster along the curve
   --  than a term does, and the node loop keeps the sum of the weights at the computed distance within
   --  Weight_Gain times the sum of the magnitudes of its terms (Is_Weight_State).

   Weight_Gain : constant := 1.0 + 7.4 * 2.0**(-53);
   --  An exact weight is at most this times the computed one, up to underflow.

   Reach_Reciprocal : constant := 2.0**100;
   --  The reciprocal of Reach.

   Gap_Dust : constant := 1.001 * 2.0**(-974);
   --  The underflow of the two doublings of a pair, which (|V - A| + B)/|V - π|² ≤ 2¹⁰¹ amplifies.

   function Weight_Value (Term : Pole_Term; Axis : Axis_Name; V : Real) return Real
   is (if Term.Imaginary_Part = 0.0
         or else
           (V - R (Term.Real_Part)) * (V - R (Term.Real_Part)) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)
           = 0.0
       then 0.0
       else
         (abs (R (Term.Residue_Real (Axis))) * abs (V - R (Term.Real_Part))
          + abs (R (Term.Residue_Imaginary (Axis))) * R (Term.Imaginary_Part))
         / ((V - R (Term.Real_Part)) * (V - R (Term.Real_Part)) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
   with Ghost;
   --  The weight of a gathered pair at V. A real pole, whose residue is gathered exactly, has none.

   function Weight_Sum (Prepared : Preparation; Count : Natural; Axis : Axis_Name; V : Real) return Real
   is (if Count = 0
       then 0.0
       else Weight_Sum (Prepared, Count - 1, Axis, V) + Weight_Value (Prepared.Terms (Count), Axis, V))
   with Ghost, Pre => Count <= Maximum_Rational_Degree, Subprogram_Variant => (Decreases => Count);
   --  The weights of the first Count gathered poles.

   function Is_Weight_State
     (Prepared : Preparation; Count : Natural; Axis : Axis_Name; U : Dimensionless; M : Real) return Boolean
   is (if Prepared.Active (Axis)
       then
         Weight_Sum (Prepared, Count, Axis, R (U)) <= Weight_Gain * M + Zeta * Poles.Slope_Sum (Prepared, Count, Axis))
   with Ghost, Pre => Count <= Maximum_Rational_Degree;
   --  What the node loop maintains besides Is_Axis_State on one axis after Count poles: the weights at the
   --  computed distance against M, the exact sum of the magnitudes of the computed terms.

   procedure Lemma_Reach_Quotient (X, D : Real)
   with Ghost, Pre => X >= 0.0 and then X * X <= D and then D >= Smallest_Square, Post => X <= Reach_Reciprocal * D;
   --  X² ≤ D and D ≥ 2⁻²⁰⁰ give X ≤ 2¹⁰⁰·D, because 2·(2¹⁰⁰·X) ≤ (2¹⁰⁰·X)² + 1.

   procedure Lemma_Reach_Quotient (X, D : Real) is
      Y : constant Real := Reach_Reciprocal * X;
   begin
      pragma Assert ((Y - 1.0) * (Y - 1.0) >= 0.0);
      pragma Assert ((Y - 1.0) * (Y - 1.0) = Y * Y - 2.0 * Y + 1.0);
      pragma Assert (Y * Y = Big * (X * X));
      Lemma_Scale (X * X, D, Big);
      pragma Assert (Big * (X * X) <= Big * D);
      pragma Assert (Big * D >= 1.0);
      pragma Assert (2.0 * Y <= 2.0 * (Big * D));
   end Lemma_Reach_Quotient;

   procedure Lemma_Doubled_Residue (Doubled, Stored : Real)
   with
     Ghost,
     Pre  => abs (Doubled - 2.0 * Stored) <= 2.0 * Epsilon * abs Stored + Eta,
     Post => abs (Doubled - 2.0 * Stored) <= Unit * abs Doubled + 1.001 * Eta;
   --  The error of a doubled residue, relative to the doubled residue instead of the stored one.

   procedure Lemma_Doubled_Residue (Doubled, Stored : Real) is
   begin
      pragma Assert (2.0 * abs Stored <= abs Doubled + abs (Doubled - 2.0 * Stored));
      pragma Assert ((2.0 - 2.0 * Epsilon) * abs Stored <= abs Doubled + Eta);
      pragma Assert (2.0 * Epsilon * abs Stored <= Unit * abs Doubled + 0.001 * Eta);
   end Lemma_Doubled_Residue;

   procedure Lemma_Pair_Gap (Rt, It, Rc, Ic, Z, B : Real)
   with
     Ghost,
     Pre  =>
       B > 0.0
       and then Z * Z + B * B >= Smallest_Square
       and then abs (Rt - 2.0 * Rc) <= 2.0 * Epsilon * abs Rc + Eta
       and then abs (It - 2.0 * Ic) <= 2.0 * Epsilon * abs Ic + Eta,
     Post =>
       abs ((Rt * Z - It * B) / (Z * Z + B * B) - 2.0 * (Rc * Z - Ic * B) / (Z * Z + B * B))
       <= Unit * ((abs Rt * abs Z + abs It * B) / (Z * Z + B * B)) + Gap_Dust;
   --  The term of a pair with its doubled residues Rt, It against the term with twice its stored residues Rc,
   --  Ic, at Z = V - A.

   procedure Lemma_Pair_Gap (Rt, It, Rc, Ic, Z, B : Real) is
      D  : constant Real := Z * Z + B * B;
      T1 : constant Real := (Rt * Z - It * B) / D;
      T2 : constant Real := 2.0 * (Rc * Z - Ic * B) / D;
      W  : constant Real := (abs Rt * abs Z + abs It * B) / D;
      Dr : constant Real := Rt - 2.0 * Rc;
      Di : constant Real := It - 2.0 * Ic;
      Er : constant Real := Unit * abs Rt + 1.001 * Eta;
      Ei : constant Real := Unit * abs It + 1.001 * Eta;
   begin
      pragma Assert (D > 0.0);
      pragma Assert (T1 * D = Rt * Z - It * B);
      pragma Assert (T2 * D = 2.0 * (Rc * Z - Ic * B));
      pragma Assert (W * D = abs Rt * abs Z + abs It * B);
      pragma Assert ((T1 - T2) * D = T1 * D - T2 * D);
      pragma Assert ((T1 - T2) * D = Dr * Z - Di * B);
      Lemma_Doubled_Residue (Rt, Rc);
      Lemma_Doubled_Residue (It, Ic);
      Lemma_Product_Bound (Dr, Z, Er, abs Z);
      Lemma_Product_Bound (Di, B, Ei, B);
      pragma Assert (abs (Dr * Z - Di * B) <= Er * abs Z + Ei * B);
      pragma Assert (Er * abs Z = Unit * (abs Rt * abs Z) + 1.001 * Eta * abs Z);
      pragma Assert (Ei * B = Unit * (abs It * B) + 1.001 * Eta * B);
      pragma Assert (Er * abs Z + Ei * B = Unit * (W * D) + 1.001 * Eta * (abs Z + B));
      --  Neither |Z| nor B is more than 2¹⁰⁰ times the squared distance.
      pragma Assert (abs Z * abs Z = Z * Z);
      pragma Assert (B * B >= 0.0 and then Z * Z >= 0.0);
      Lemma_Reach_Quotient (abs Z, D);
      Lemma_Reach_Quotient (B, D);
      pragma Assert (1.001 * Eta * (abs Z + B) <= Gap_Dust * D);
      pragma Assert (abs ((T1 - T2) * D) = abs (T1 - T2) * D);
      pragma Assert ((Unit * W + Gap_Dust) * D = Unit * (W * D) + Gap_Dust * D);
      pragma Assert (abs (T1 - T2) * D <= (Unit * W + Gap_Dust) * D);
      Lemma_Cancel (abs (T1 - T2), Unit * W + Gap_Dust, D);
   end Lemma_Pair_Gap;

   procedure Lemma_Pair_Term_Gap
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term; Axis : Axis_Name; V : Real)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Pair <= Cache.Pair_Count
       and then Poles.Is_Pair_Term (Cache, Pair, Term)
       and then Poles.Is_Term_Bounded (Term)
       and then V >= 0.0
       and then V <= 1.0,
     Post =>
       abs (Term_Value (Term, Axis, V) - Pair_Term (Cache, Pair, Axis, V))
       <= Unit * Weight_Value (Term, Axis, V) + Gap_Dust;
   --  The term of a gathered pair against the term of the pair in the cache.

   procedure Lemma_Pair_Term_Gap
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term; Axis : Axis_Name; V : Real)
   is
      A  : constant Real := R (Term.Real_Part);
      B  : constant Real := R (Term.Imaginary_Part);
      Z  : constant Real := V - A;
      D  : constant Real := Z * Z + B * B;
      H  : constant Real := Poles.Horizontal_Distance (A);
      Rt : constant Real := R (Term.Residue_Real (Axis));
      It : constant Real := R (Term.Residue_Imaginary (Axis));
      Rc : constant Real := R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis));
      Ic : constant Real := R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis));
   begin
      pragma Assert (Term.Imaginary_Part > 0.0);
      pragma Assert (B > 0.0);
      Lemma_Horizontal_Reach (A, V);
      pragma Assert (Poles.Distance_Square (Term) = H * H + B * B);
      pragma Assert (Poles.Distance_Square (Term) >= Smallest_Square);
      pragma Assert (D >= Smallest_Square);
      pragma Assert (Pair_Distance_Square (Cache, Pair, V) = D);
      pragma Assert (abs (Rt - 2.0 * Rc) <= 2.0 * Epsilon * abs Rc + Eta);
      pragma Assert (abs (It - 2.0 * Ic) <= 2.0 * Epsilon * abs Ic + Eta);
      Lemma_Pair_Gap (Rt, It, Rc, Ic, Z, B);
      pragma Assert (Term_Value (Term, Axis, V) = (Rt * Z - It * B) / D);
      pragma Assert (Pair_Term (Cache, Pair, Axis, V) = 2.0 * (Rc * Z - Ic * B) / D);
      pragma Assert (Weight_Value (Term, Axis, V) = (abs Rt * abs Z + abs It * B) / D);
   end Lemma_Pair_Term_Gap;

   procedure Lemma_Term_Sum_Gap
     (Cache : Rational_Antiderivative; Prepared : Preparation; Count : Natural; Axis : Axis_Name; V : Real)
   with
     Ghost,
     Pre                =>
       Has_Layout (Cache)
       and then Poles.Is_Cache_Prepared (Cache, Prepared)
       and then Count <= Prepared.Count
       and then V >= 0.0
       and then V <= 1.0,
     Post               =>
       abs (Term_Sum (Prepared, Count, Axis, V)
            - (Real_Sum (Cache, Real_Count (Cache, Count), Axis, V)
               + Pair_Sum (Cache, Pair_Count (Cache, Count), Axis, V)))
       <= Unit * Weight_Sum (Prepared, Count, Axis, V) + To_Real (Count) * Gap_Dust,
     Subprogram_Variant => (Decreases => Count);
   --  The gathered terms against those of the cache.

   procedure Lemma_Term_Sum_Gap
     (Cache : Rational_Antiderivative; Prepared : Preparation; Count : Natural; Axis : Axis_Name; V : Real) is
   begin
      if Count = 0 then
         pragma Assert (Term_Sum (Prepared, 0, Axis, V) = 0.0);
         pragma Assert (Weight_Sum (Prepared, 0, Axis, V) = 0.0);
         pragma Assert (Real_Sum (Cache, 0, Axis, V) = 0.0);
         pragma Assert (Pair_Sum (Cache, 0, Axis, V) = 0.0);
         pragma Assert (To_Real (0) = 0.0);
         return;
      end if;
      Lemma_Term_Sum_Gap (Cache, Prepared, Count - 1, Axis, V);
      Lemma_Term_Kind (Cache, Prepared, Count);
      pragma Assert (To_Real (Count) = To_Real (Count - 1) + 1.0);
      pragma Assert (To_Real (Count) * Gap_Dust = To_Real (Count - 1) * Gap_Dust + Gap_Dust);
      pragma
        Assert
          (Term_Sum (Prepared, Count, Axis, V)
           = Term_Sum (Prepared, Count - 1, Axis, V) + Term_Value (Prepared.Terms (Count), Axis, V));
      pragma
        Assert
          (Weight_Sum (Prepared, Count, Axis, V)
           = Weight_Sum (Prepared, Count - 1, Axis, V) + Weight_Value (Prepared.Terms (Count), Axis, V));
      pragma
        Assert
          (Unit * Weight_Sum (Prepared, Count, Axis, V)
           = Unit * Weight_Sum (Prepared, Count - 1, Axis, V) + Unit * Weight_Value (Prepared.Terms (Count), Axis, V));
      if Count <= Cache.Real_Pole_Count then
         pragma Assert (Term_Value (Prepared.Terms (Count), Axis, V) = Real_Term (Cache, Count, Axis, V));
         pragma Assert (Weight_Value (Prepared.Terms (Count), Axis, V) = 0.0);
         pragma
           Assert
             (Real_Sum (Cache, Count, Axis, V)
              = Real_Sum (Cache, Count - 1, Axis, V) + Real_Term (Cache, Count, Axis, V));
         pragma Assert (Pair_Count (Cache, Count) = 0 and then Pair_Count (Cache, Count - 1) = 0);
         pragma Assert (Real_Count (Cache, Count) = Count and then Real_Count (Cache, Count - 1) = Count - 1);
      else
         declare
            Pair : constant Rational_Pair_Index := Count - Cache.Real_Pole_Count;
         begin
            Lemma_Pair_Term_Gap (Cache, Pair, Prepared.Terms (Count), Axis, V);
            pragma
              Assert
                (Pair_Sum (Cache, Pair, Axis, V)
                 = Pair_Sum (Cache, Pair - 1, Axis, V) + Pair_Term (Cache, Pair, Axis, V));
            pragma Assert (Pair_Count (Cache, Count) = Pair and then Pair_Count (Cache, Count - 1) = Pair - 1);
            pragma Assert (Real_Count (Cache, Count) = Cache.Real_Pole_Count);
            pragma Assert (Real_Count (Cache, Count - 1) = Cache.Real_Pole_Count);
         end;
      end if;
   end Lemma_Term_Sum_Gap;

   procedure Lemma_Weight_Move (Ar, Ai, B, Z1, Z2, Square : Real)
   with
     Ghost,
     Pre  =>
       Ar >= 0.0
       and then Ai >= 0.0
       and then Square > 0.0
       and then Square <= Z1 * Z1 + B * B
       and then Square <= Z2 * Z2 + B * B,
     Post =>
       abs ((Ar * abs Z1 + Ai * B) / (Z1 * Z1 + B * B) - (Ar * abs Z2 + Ai * B) / (Z2 * Z2 + B * B)) * Square
       <= (Ar + Ai) * abs (Z1 - Z2);
   --  A weight is the term of a pair with residues Ar and -Ai at |Z|, and |Z| moves no faster than Z.

   procedure Lemma_Weight_Move (Ar, Ai, B, Z1, Z2, Square : Real) is
      Y1 : constant Real := abs Z1;
      Y2 : constant Real := abs Z2;
      W1 : constant Real := (Ar * abs Z1 + Ai * B) / (Z1 * Z1 + B * B);
      W2 : constant Real := (Ar * abs Z2 + Ai * B) / (Z2 * Z2 + B * B);
   begin
      pragma Assert (Y1 * Y1 = Z1 * Z1);
      pragma Assert (Y2 * Y2 = Z2 * Z2);
      Lemma_Pair_Shift (Ar, -Ai, B, Y1, Y2, Square);
      pragma Assert (Ar * Y1 - (-Ai) * B = Ar * abs Z1 + Ai * B);
      pragma Assert (Ar * Y2 - (-Ai) * B = Ar * abs Z2 + Ai * B);
      pragma Assert ((Ar * Y1 - (-Ai) * B) / (Y1 * Y1 + B * B) = W1);
      pragma Assert ((Ar * Y2 - (-Ai) * B) / (Y2 * Y2 + B * B) = W2);
      pragma Assert (abs Ar + abs (-Ai) = Ar + Ai);
      pragma Assert (abs (W1 - W2) * Square <= (Ar + Ai) * abs (Y1 - Y2));
      pragma Assert (abs (Y1 - Y2) <= abs (Z1 - Z2));
      Lemma_Scale (abs (Y1 - Y2), abs (Z1 - Z2), Ar + Ai);
   end Lemma_Weight_Move;

   procedure Lemma_Term_Weight_Shift (Term : Pole_Term; Axis : Axis_Name; V1, V2 : Real)
   with
     Ghost,
     Pre  => Is_Term_Ready (Term) and then V1 >= 0.0 and then V1 <= 1.0 and then V2 >= 0.0 and then V2 <= 1.0,
     Post =>
       abs (Weight_Value (Term, Axis, V1) - Weight_Value (Term, Axis, V2))
       <= abs (V1 - V2) * Poles.Slope_Term (Term, Axis);
   --  On [0, 1] the weight of a pair changes no faster than |r|/d².

   procedure Lemma_Term_Weight_Shift (Term : Pole_Term; Axis : Axis_Name; V1, V2 : Real) is
      A      : constant Real := R (Term.Real_Part);
      B      : constant Real := R (Term.Imaginary_Part);
      Ar     : constant Real := abs (R (Term.Residue_Real (Axis)));
      Ai     : constant Real := abs (R (Term.Residue_Imaginary (Axis)));
      H      : constant Real := Poles.Horizontal_Distance (A);
      Square : constant Real := Poles.Distance_Square (Term);
      Z1     : constant Real := V1 - A;
      Z2     : constant Real := V2 - A;
      Change : constant Real := abs (Weight_Value (Term, Axis, V1) - Weight_Value (Term, Axis, V2));
      Slope  : constant Real := Poles.Slope_Term (Term, Axis);
   begin
      pragma Assert (Square >= Smallest_Square);
      pragma Assert (Square = H * H + B * B);
      pragma Assert (Slope = (Ar + Ai) / Square);
      pragma Assert (Slope * Square = Ar + Ai);
      pragma Assert (Slope >= 0.0);
      if Term.Imaginary_Part = 0.0 then
         pragma Assert (Weight_Value (Term, Axis, V1) = 0.0);
         pragma Assert (Weight_Value (Term, Axis, V2) = 0.0);
         Lemma_Scale (0.0, abs (V1 - V2), Slope);
      else
         Lemma_Horizontal_Reach (A, V1);
         Lemma_Horizontal_Reach (A, V2);
         pragma Assert (B > 0.0);
         pragma Assert (B * B > 0.0);
         pragma Assert (Z1 * Z1 + B * B >= Square);
         pragma Assert (Z2 * Z2 + B * B >= Square);
         Lemma_Weight_Move (Ar, Ai, B, Z1, Z2, Square);
         pragma Assert (Weight_Value (Term, Axis, V1) = (Ar * abs Z1 + Ai * B) / (Z1 * Z1 + B * B));
         pragma Assert (Weight_Value (Term, Axis, V2) = (Ar * abs Z2 + Ai * B) / (Z2 * Z2 + B * B));
         pragma Assert (Z1 - Z2 = V1 - V2);
         pragma Assert (Change * Square <= (Ar + Ai) * abs (V1 - V2));
         Lemma_Quotient_Bound (Change, Ar + Ai, Square, abs (V1 - V2));
      end if;
   end Lemma_Term_Weight_Shift;

   procedure Lemma_Weight_Sum_Shift
     (Cache : Rational_Antiderivative; Prepared : Preparation; Count : Natural; Axis : Axis_Name; V1, V2 : Real)
   with
     Ghost,
     Pre                =>
       Has_Layout (Cache)
       and then Poles.Is_Cache_Prepared (Cache, Prepared)
       and then Count <= Prepared.Count
       and then V1 >= 0.0
       and then V1 <= 1.0
       and then V2 >= 0.0
       and then V2 <= 1.0,
     Post               =>
       abs (Weight_Sum (Prepared, Count, Axis, V1) - Weight_Sum (Prepared, Count, Axis, V2))
       <= abs (V1 - V2) * Poles.Slope_Sum (Prepared, Count, Axis),
     Subprogram_Variant => (Decreases => Count);
   --  The same for the sum of the weights.

   procedure Lemma_Weight_Sum_Shift
     (Cache : Rational_Antiderivative; Prepared : Preparation; Count : Natural; Axis : Axis_Name; V1, V2 : Real) is
   begin
      if Count = 0 then
         pragma Assert (Weight_Sum (Prepared, 0, Axis, V1) = 0.0);
         pragma Assert (Weight_Sum (Prepared, 0, Axis, V2) = 0.0);
         pragma Assert (Poles.Slope_Sum (Prepared, 0, Axis) = 0.0);
         return;
      end if;
      Lemma_Weight_Sum_Shift (Cache, Prepared, Count - 1, Axis, V1, V2);
      Lemma_Term_Kind (Cache, Prepared, Count);
      pragma Assert (Is_Term_Ready (Prepared.Terms (Count)));
      Lemma_Term_Weight_Shift (Prepared.Terms (Count), Axis, V1, V2);
      pragma
        Assert
          (Weight_Sum (Prepared, Count, Axis, V1)
           = Weight_Sum (Prepared, Count - 1, Axis, V1) + Weight_Value (Prepared.Terms (Count), Axis, V1));
      pragma
        Assert
          (Weight_Sum (Prepared, Count, Axis, V2)
           = Weight_Sum (Prepared, Count - 1, Axis, V2) + Weight_Value (Prepared.Terms (Count), Axis, V2));
      pragma
        Assert
          (Poles.Slope_Sum (Prepared, Count, Axis)
           = Poles.Slope_Sum (Prepared, Count - 1, Axis) + Poles.Slope_Term (Prepared.Terms (Count), Axis));
      pragma
        Assert
          (abs (V1 - V2) * Poles.Slope_Sum (Prepared, Count, Axis)
           = abs (V1 - V2) * Poles.Slope_Sum (Prepared, Count - 1, Axis)
             + abs (V1 - V2) * Poles.Slope_Term (Prepared.Terms (Count), Axis));
   end Lemma_Weight_Sum_Shift;

   procedure Lemma_Weight_Start (Prepared : Preparation; Axis : Axis_Name; U : Dimensionless)
   with Ghost, Post => Is_Weight_State (Prepared, 0, Axis, U, 0.0);
   --  Before the first pole.

   procedure Lemma_Weight_Start (Prepared : Preparation; Axis : Axis_Name; U : Dimensionless) is
   begin
      pragma Assert (Weight_Sum (Prepared, 0, Axis, R (U)) = 0.0);
      pragma Assert (Poles.Slope_Sum (Prepared, 0, Axis) = 0.0);
   end Lemma_Weight_Start;

   procedure Lemma_Real_Weight_Step
     (Prepared : Preparation; Index : Rational_Degree_Slot; Axis : Axis_Name; U : Dimensionless; M, Mu : Real)
   with
     Ghost,
     Pre  =>
       Is_Term_Ready (Prepared.Terms (Index))
       and then Prepared.Terms (Index).Imaginary_Part = 0.0
       and then Mu >= 0.0
       and then Is_Weight_State (Prepared, Index - 1, Axis, U, M),
     Post => Is_Weight_State (Prepared, Index, Axis, U, M + Mu);
   --  A real pole adds no weight.

   procedure Lemma_Real_Weight_Step
     (Prepared : Preparation; Index : Rational_Degree_Slot; Axis : Axis_Name; U : Dimensionless; M, Mu : Real)
   is
      Term : constant Pole_Term := Prepared.Terms (Index);
      Sl   : constant Real := Poles.Slope_Term (Term, Axis);
   begin
      Lemma_Slope_Term_Sign (Term, Axis);
      pragma Assert (Sl >= 0.0);
      if Prepared.Active (Axis) then
         pragma Assert (Weight_Value (Term, Axis, R (U)) = 0.0);
         pragma
           Assert
             (Weight_Sum (Prepared, Index, Axis, R (U))
              = Weight_Sum (Prepared, Index - 1, Axis, R (U)) + Weight_Value (Term, Axis, R (U)));
         pragma Assert (Poles.Slope_Sum (Prepared, Index, Axis) = Poles.Slope_Sum (Prepared, Index - 1, Axis) + Sl);
         pragma Assert (Weight_Gain * (M + Mu) = Weight_Gain * M + Weight_Gain * Mu);
         pragma
           Assert
             (Zeta * Poles.Slope_Sum (Prepared, Index, Axis)
              = Zeta * Poles.Slope_Sum (Prepared, Index - 1, Axis) + Zeta * Sl);
      end if;
   end Lemma_Real_Weight_Step;

   procedure Lemma_Pair_Weight_Bound (Rr, Ri, Wr, Wi, Z, B, RD, Sl : Real)
   with
     Ghost,
     Pre  =>
       RD > 0.0
       and then B > 0.0
       and then Wi >= 0.0
       and then abs (Wr - Z * RD) <= Weight_Error * (abs Z * RD) + 1.01 * Eta * RD + Eta
       and then abs (Wi - B * RD) <= Weight_Error * (B * RD) + Eta
       and then Sl >= 0.0
       and then abs Rr * RD <= Sl
       and then abs Rr + abs Ri <= Slope_Reach * Sl,
     Post => abs Rr * (abs Z * RD) + abs Ri * (B * RD) <= Weight_Gain * (abs Rr * abs Wr + abs Ri * Wi) + Zeta * Sl;
   --  The exact weight of a pair against the magnitude of its computed term: the errors of the two computed
   --  weights, and the underflows which the residues amplify, which a sliver of the slope covers.

   procedure Lemma_Pair_Weight_Bound (Rr, Ri, Wr, Wi, Z, B, RD, Sl : Real) is
      Tr   : constant Real := abs Z * RD;
      Ti   : constant Real := B * RD;
      Slip : constant Real := 1.01 * Eta * RD + Eta;
      Ar   : constant Real := abs Rr;
      Ai   : constant Real := abs Ri;
   begin
      Lemma_Scale (0.0, abs Z, RD);
      Lemma_Scale (0.0, B, RD);
      pragma Assert (Tr >= 0.0 and then Ti >= 0.0);
      pragma Assert (abs (Z * RD) = Tr);
      pragma Assert (Tr <= abs Wr + abs (Wr - Z * RD));
      pragma Assert ((1.0 - Weight_Error) * Tr <= abs Wr + Slip);
      pragma Assert (Tr <= Weight_Gain * (abs Wr + Slip));
      pragma Assert (Ti <= Wi + abs (Wi - Ti));
      pragma Assert ((1.0 - Weight_Error) * Ti <= Wi + Eta);
      pragma Assert (Ti <= Weight_Gain * (Wi + Eta));
      Lemma_Scale (Tr, Weight_Gain * (abs Wr + Slip), Ar);
      Lemma_Scale (Ti, Weight_Gain * (Wi + Eta), Ai);
      pragma Assert ((Weight_Gain * (abs Wr + Slip)) * Ar = Weight_Gain * (Ar * abs Wr) + Weight_Gain * (Ar * Slip));
      pragma Assert ((Weight_Gain * (Wi + Eta)) * Ai = Weight_Gain * (Ai * Wi) + Weight_Gain * (Ai * Eta));
      pragma
        Assert
          (Ar * Tr + Ai * Ti
           <= Weight_Gain * (Ar * abs Wr + Ai * Wi) + Weight_Gain * (Ar * Slip) + Weight_Gain * (Ai * Eta));
      --  The underflows, as in Lemma_Pair_Term_Error.
      pragma Assert (Slip >= 0.0);
      Lemma_Scale (0.0, Ar, Slip);
      pragma Assert (Ar * Slip >= 0.0 and then Ai * Eta >= 0.0);
      pragma Assert (Weight_Gain * (Ar * Slip) + Weight_Gain * (Ai * Eta) <= 1.01 * (Ar * Slip) + 1.01 * (Ai * Eta));
      pragma Assert (Ar * Slip = 1.01 * Eta * (Ar * RD) + Eta * Ar);
      pragma Assert (Ar * Slip <= 1.01 * Eta * Sl + Eta * Ar);
      pragma Assert (1.01 * (Ar * Slip) + 1.01 * (Ai * Eta) <= 1.03 * Eta * Sl + 1.01 * Eta * (Ar + Ai));
      pragma Assert (1.01 * Eta * (Ar + Ai) <= 1.01 * Eta * (Slope_Reach * Sl));
      pragma Assert (1.03 * Eta * Sl + 1.01 * Eta * (Slope_Reach * Sl) = (1.03 * Eta + 1.01 * Eta * Slope_Reach) * Sl);
      Lemma_Scale (1.03 * Eta + 1.01 * Eta * Slope_Reach, Zeta, Sl);
      pragma Assert (1.01 * (Ar * Slip) + 1.01 * (Ai * Eta) <= Zeta * Sl);
   end Lemma_Pair_Weight_Bound;

   procedure Lemma_Pair_Weight_Step
     (Prepared : Preparation;
      Index    : Rational_Degree_Slot;
      Axis     : Axis_Name;
      U        : Dimensionless;
      Wr, Wi   : Dimensionless;
      Inv, M   : Real)
   with
     Ghost,
     Pre  =>
       U >= 0.0
       and then U <= 1.0
       and then Is_Term_Ready (Prepared.Terms (Index))
       and then Prepared.Terms (Index).Imaginary_Part > 0.0
       and then Wi >= 0.0
       and then
         Is_Pair_Weight
           (R (U) - R (Prepared.Terms (Index).Real_Part),
            R (Prepared.Terms (Index).Imaginary_Part),
            Inv,
            R (Wr),
            R (Wi))
       and then Is_Weight_State (Prepared, Index - 1, Axis, U, M),
     Post =>
       Is_Weight_State
         (Prepared,
          Index,
          Axis,
          U,
          M
          + (abs (R (Prepared.Terms (Index).Residue_Real (Axis))) * abs (R (Wr))
             + abs (R (Prepared.Terms (Index).Residue_Imaginary (Axis))) * R (Wi)));
   --  One pair more in the weights of one axis.

   procedure Lemma_Pair_Weight_Step
     (Prepared : Preparation;
      Index    : Rational_Degree_Slot;
      Axis     : Axis_Name;
      U        : Dimensionless;
      Wr, Wi   : Dimensionless;
      Inv, M   : Real)
   is
      Term  : constant Pole_Term := Prepared.Terms (Index);
      Ar    : constant Real := abs (R (Term.Residue_Real (Axis)));
      Ai    : constant Real := abs (R (Term.Residue_Imaginary (Axis)));
      A     : constant Real := R (Term.Real_Part);
      B     : constant Real := R (Term.Imaginary_Part);
      Z     : constant Real := R (U) - A;
      D     : constant Real := Z * Z + B * B;
      RD    : constant Real := 1.0 / D;
      Mu    : constant Real := Ar * abs (R (Wr)) + Ai * R (Wi);
      Sl    : constant Real := Poles.Slope_Term (Term, Axis);
      Omega : constant Real := Weight_Value (Term, Axis, R (U));
   begin
      pragma Assert (Poles.Is_Term_Bounded (Term));
      pragma Assert (R (U) >= 0.0 and then R (U) <= 1.0);
      Lemma_Pair_Slope (Term, Axis, R (U));
      pragma Assert (D >= Smallest_Square);
      Lemma_Pair_Weight_Elim (Z, B, Inv, R (Wr), R (Wi));
      Lemma_Reciprocal (D);
      pragma Assert (RD > 0.0);
      pragma Assert (R (Wi) >= 0.0);
      pragma Assert (abs (Ar) = Ar and then abs (Ai) = Ai);
      Lemma_Pair_Weight_Bound
        (R (Term.Residue_Real (Axis)), R (Term.Residue_Imaginary (Axis)), R (Wr), R (Wi), Z, B, RD, Sl);
      Lemma_Division (Ar * abs Z + Ai * B, D);
      pragma Assert ((Ar * abs Z + Ai * B) * RD = Ar * (abs Z * RD) + Ai * (B * RD));
      pragma Assert (Omega = (Ar * abs Z + Ai * B) / D);
      pragma Assert (Omega <= Weight_Gain * Mu + Zeta * Sl);
      if Prepared.Active (Axis) then
         pragma
           Assert (Weight_Sum (Prepared, Index, Axis, R (U)) = Weight_Sum (Prepared, Index - 1, Axis, R (U)) + Omega);
         pragma Assert (Poles.Slope_Sum (Prepared, Index, Axis) = Poles.Slope_Sum (Prepared, Index - 1, Axis) + Sl);
         pragma Assert (Weight_Gain * (M + Mu) = Weight_Gain * M + Weight_Gain * Mu);
         pragma
           Assert
             (Zeta * Poles.Slope_Sum (Prepared, Index, Axis)
              = Zeta * Poles.Slope_Sum (Prepared, Index - 1, Axis) + Zeta * Sl);
      end if;
   end Lemma_Pair_Weight_Step;

   ---------------------------------------------------------------------------
   --  The factors of the cached denominator
   ---------------------------------------------------------------------------

   Lambda : constant := 40.0 * 2.0**(-53);
   --  The computed partial product is within this times its amplification, less the initial sixteen, of the
   --  exact one, relatively.

   Factor_Error : constant := 38.0 * 2.0**(-53);
   --  The relative error of one computed factor is at most this times its share of the amplification.

   Amplification_Bound : constant := 2.0**35;
   --  Largest_Amplification, as a real number.

   Share_Bound : constant := 2.0**36;
   --  No share of an amplification within Largest_Amplification is larger.

   Small_Error : constant := 2.0**(-11);
   --  More than Lambda or Factor_Error times any such amplification or share.

   Amplification_Step : constant := 2.0**201;
   --  More than one pole can add to the amplification.

   Gain_Loss : constant := 2.0**(-15);
   --  The fraction of a pole's share which the rounding of the amplification can lose.

   Smallest_Reciprocal : constant := 2.0**(-201);
   --  Less than the reciprocal of any magnitude within Magnitude_Limit.

   procedure Lemma_Relative_Swap (A, B, E, E2 : Real)
   with
     Ghost,
     Pre  => abs (A - B) <= E * abs A and then E >= 0.0 and then E < 1.0 and then E2 * (1.0 - E) >= E,
     Post => abs (A - B) <= E2 * abs B;
   --  An error relative to one of two close numbers, relative to the other.

   procedure Lemma_Relative_Swap (A, B, E, E2 : Real) is
   begin
      pragma Assert (abs A <= abs B + abs (A - B));
      Lemma_Scale (abs A, abs B + abs (A - B), E);
      pragma Assert (E * (abs B + abs (A - B)) = E * abs B + E * abs (A - B));
      pragma Assert ((1.0 - E) * abs (A - B) <= E * abs B);
      Lemma_Scale (E, E2 * (1.0 - E), abs B);
      pragma Assert ((E2 * (1.0 - E)) * abs B = (E2 * abs B) * (1.0 - E));
      pragma Assert (abs (A - B) * (1.0 - E) <= (E2 * abs B) * (1.0 - E));
      Lemma_Cancel (abs (A - B), E2 * abs B, 1.0 - E);
   end Lemma_Relative_Swap;

   procedure Lemma_Compound (Qh, P, Fh, F, Qn, Rho, Ef : Real)
   with
     Ghost,
     Pre  =>
       abs (Qh - P) <= Rho * abs P
       and then Rho >= 0.0
       and then Rho <= Small_Error
       and then abs (Fh - F) <= Ef * abs F
       and then Ef >= 0.0
       and then Ef <= Small_Error
       and then abs (Qn - Qh * Fh) <= Unit * abs (Qh * Fh),
     Post => abs (Qn - P * F) <= (Rho + 1.0005 * Ef + 1.001 * Unit) * abs (P * F);
   --  One more factor: the relative errors of the partial product, of the factor and of their rounded product
   --  add up, and their products are a small fraction more.

   procedure Lemma_Compound (Qh, P, Fh, F, Qn, Rho, Ef : Real) is
      E2 : constant Real := Rho + Ef + Rho * Ef;
      E3 : constant Real := Unit + E2 + Unit * E2;
   begin
      Lemma_Relative_Product (Qh, P, Fh, F, Rho, Ef, E2);
      Lemma_Relative_Chain (Qn, Qh * Fh, P * F, Unit, E2, E3);
      Lemma_Scale (Rho, Small_Error, Ef);
      pragma Assert (Rho * Ef <= 0.0005 * Ef);
      pragma Assert (E2 <= Rho + 1.0005 * Ef);
      pragma Assert (E2 >= 0.0);
      pragma Assert (E2 <= 0.001);
      Lemma_Scale (E2, 0.001, Unit);
      pragma Assert (Unit * E2 <= 0.001 * Unit);
      pragma Assert (E3 <= Rho + 1.0005 * Ef + 1.001 * Unit);
      Lemma_Scale (E3, Rho + 1.0005 * Ef + 1.001 * Unit, abs (P * F));
   end Lemma_Compound;

   procedure Lemma_Maximum_Value (Value : Dimensionless)
   with
     Ghost,
     Pre  => Value >= 0.0 and then Value <= 2.0**202,
     Post =>
       (if R (Value) >= 1.0
        then R (Dimensionless'Max (1.0, Value)) = R (Value)
        else R (Dimensionless'Max (1.0, Value)) = 1.0);

   procedure Lemma_Maximum_Value (Value : Dimensionless) is null;

   procedure Lemma_Maximum (Value : Dimensionless)
   with
     Ghost,
     Pre  => Value >= 0.0 and then Value <= 2.0**202,
     Post =>
       Dimensionless'Max (1.0, Value) >= 1.0
       and then Dimensionless'Max (1.0, Value) <= 2.0**202
       and then R (Dimensionless'Max (1.0, Value)) >= 1.0
       and then R (Dimensionless'Max (1.0, Value)) >= R (Value)
       and then (R (Dimensionless'Max (1.0, Value)) = 1.0 or else R (Dimensionless'Max (1.0, Value)) = R (Value));
   --  A pole's share of the amplification.

   procedure Lemma_Maximum (Value : Dimensionless) is
   begin
      Lemma_Maximum_Value (Value);
   end Lemma_Maximum;

   procedure Lemma_Amplification (Amp, Share : Dimensionless; Count : Positive)
   with
     Ghost,
     Pre  =>
       Count <= Maximum_Rational_Degree
       and then Amp >= 16.0
       and then R (Amp) <= 16.0 + To_Real (Count - 1) * Amplification_Step
       and then Share >= 1.0
       and then R (Share) <= Inverse_Limit,
     Post =>
       Amp <= 2.0**206
       and then Share <= 2.0**201
       and then Amp + Share >= 16.0
       and then Amp + Share >= Amp
       and then R (Amp + Share) <= 16.0 + To_Real (Count) * Amplification_Step
       and then abs (R (Amp + Share) - (R (Amp) + R (Share))) <= Epsilon * (R (Amp) + R (Share)) + Eta;
   --  The amplification stays finite, and never decreases.

   procedure Lemma_Amplification (Amp, Share : Dimensionless; Count : Positive) is
   begin
      Lemma_Coefficients (Count);
      pragma Assert (R (Amp) <= 16.0 + 15.0 * Amplification_Step);
      pragma Assert (Amp <= 2.0**206);
      pragma Assert (Share <= 2.0**201);
      Float_Lemmas.Lemma_Rounding_Error_Add (Amp, Share);
      pragma Assert (R (Amp) >= 16.0 and then R (Share) >= 1.0);
      pragma Assert (R (Amp) + R (Share) <= 16.0 + To_Real (Count - 1) * Amplification_Step + Inverse_Limit);
      pragma Assert (Epsilon * (R (Amp) + R (Share)) + Eta <= 0.25 * Amplification_Step);
      pragma Assert (R (Amp + Share) <= 16.0 + To_Real (Count) * Amplification_Step);
      pragma Assert (Amp + Share >= 16.0);
      pragma Assert (Amp + Share >= Amp);
   end Lemma_Amplification;

   procedure Lemma_Amplification_Gain (A, S, An : Real)
   with
     Ghost,
     Pre  =>
       A >= 16.0
       and then S >= 1.0
       and then abs (An - (A + S)) <= Epsilon * (A + S) + Eta
       and then An <= Amplification_Bound,
     Post =>
       A <= Amplification_Bound and then S <= Share_Bound and then An - 16.0 >= (A - 16.0) + (1.0 - Gain_Loss) * S;
   --  While it is within Largest_Amplification, the rounding of the amplification loses next to nothing of a
   --  pole's share.

   procedure Lemma_Amplification_Gain (A, S, An : Real) is
      pragma Unreferenced (An);
   begin
      pragma Assert ((1.0 - Epsilon) * (A + S) <= Amplification_Bound + Eta);
      pragma Assert (A + S <= Share_Bound);
      pragma Assert (Epsilon * (A + S) + Eta <= Gain_Loss);
      pragma Assert (Gain_Loss <= Gain_Loss * S);
   end Lemma_Amplification_Gain;

   function Is_Q_State
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      V                 : Real;
      Accurate          : Boolean;
      Count             : Natural;
      Q, Amp            : Dimensionless) return Boolean
   with
     Ghost,
     Pre  => Has_Layout (Cache) and then Count <= Cache.Real_Pole_Count + Cache.Pair_Count,
     Post =>
       Is_Q_State'Result
       = (if Accurate and then Amp <= Largest_Amplification
          then
            abs (R (Q) - Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V))
            <= Lambda * (R (Amp) - 16.0) * abs (Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V)));
   --  What the node loop maintains of the running product after Count poles, at an accurate sample and while
   --  the amplification is within Largest_Amplification.

   function Is_Q_State
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      V                 : Real;
      Accurate          : Boolean;
      Count             : Natural;
      Q, Amp            : Dimensionless) return Boolean is
   begin
      return
        (if Accurate and then Amp <= Largest_Amplification
         then
           abs (R (Q) - Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V))
           <= Lambda * (R (Amp) - 16.0) * abs (Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V)));
   end Is_Q_State;

   procedure Lemma_Q_State_Intro
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      V                 : Real;
      Accurate          : Boolean;
      Count             : Natural;
      Q, Amp            : Dimensionless)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Count <= Cache.Real_Pole_Count + Cache.Pair_Count
       and then
         (if Accurate and then Amp <= Largest_Amplification
          then
            abs (R (Q) - Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V))
            <= Lambda * (R (Amp) - 16.0) * abs (Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V))),
     Post => Is_Q_State (Warp, Cache, Denominator_Scale, V, Accurate, Count, Q, Amp);

   procedure Lemma_Q_State_Intro
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      V                 : Real;
      Accurate          : Boolean;
      Count             : Natural;
      Q, Amp            : Dimensionless)
   is null;

   procedure Lemma_Q_State_Elim
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      V                 : Real;
      Count             : Natural;
      Q, Amp            : Dimensionless)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Count <= Cache.Real_Pole_Count + Cache.Pair_Count
       and then Amp <= Largest_Amplification
       and then Is_Q_State (Warp, Cache, Denominator_Scale, V, True, Count, Q, Amp),
     Post =>
       abs (R (Q) - Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V))
       <= Lambda * (R (Amp) - 16.0) * abs (Partial_Denominator (Cache, Denominator_Scale, Warp, Count, V));

   procedure Lemma_Q_State_Elim
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      V                 : Real;
      Count             : Natural;
      Q, Amp            : Dimensionless)
   is null;

   procedure Lemma_Q_Step (Qh, P, Fh, F, Qn, A, S, An : Real)
   with
     Ghost,
     Pre  =>
       A >= 16.0
       and then A <= Amplification_Bound
       and then S >= 1.0
       and then S <= Share_Bound
       and then An - 16.0 >= (A - 16.0) + (1.0 - Gain_Loss) * S
       and then abs (Qh - P) <= Lambda * (A - 16.0) * abs P
       and then abs (Fh - F) <= Factor_Error * S * abs F
       and then abs (Qn - Qh * Fh) <= Epsilon * abs (Qh * Fh) + Eta
       and then abs Qn >= 2.0 * Normal_Floor,
     Post => abs (Qn - P * F) <= Lambda * (An - 16.0) * abs (P * F);
   --  The running product after one more factor, whose relative error is at most Factor_Error times its share
   --  S of the amplification.

   procedure Lemma_Q_Step (Qh, P, Fh, F, Qn, A, S, An : Real) is
      Rho : constant Real := Lambda * (A - 16.0);
      Ef  : constant Real := Factor_Error * S;
   begin
      pragma Assert (Rho >= 0.0 and then Rho <= Small_Error);
      pragma Assert (Ef >= 0.0 and then Ef <= Small_Error);
      pragma Assert (abs (Qh * Fh) >= Normal_Floor);
      Lemma_Normal_Rounding (Qn, Qh * Fh);
      Lemma_Compound (Qh, P, Fh, F, Qn, Rho, Ef);
      pragma Assert (Rho + 1.0005 * Ef + 1.001 * Unit <= Lambda * (An - 16.0));
      Lemma_Scale (Rho + 1.0005 * Ef + 1.001 * Unit, Lambda * (An - 16.0), abs (P * F));
   end Lemma_Q_Step;

   procedure Lemma_Share (Amp, Share : Dimensionless)
   with
     Ghost,
     Pre  =>
       Amp >= 16.0
       and then Amp <= 2.0**206
       and then Share >= 1.0
       and then Share <= 2.0**201
       and then Amp + Share <= Largest_Amplification,
     Post =>
       R (Amp) >= 16.0
       and then R (Amp) <= Amplification_Bound
       and then R (Share) >= 1.0
       and then R (Share) <= Share_Bound
       and then R (Amp + Share) - 16.0 >= (R (Amp) - 16.0) + (1.0 - Gain_Loss) * R (Share);
   --  An amplification within Largest_Amplification, and the share of the pole which was added last.

   procedure Lemma_Share (Amp, Share : Dimensionless) is
   begin
      Float_Lemmas.Lemma_Rounding_Error_Add (Amp, Share);
      pragma Assert (R (Amp) >= 16.0 and then R (Share) >= 1.0);
      pragma Assert (R (Amp + Share) <= Amplification_Bound);
      pragma Assert (abs (R (Amp + Share) - (R (Amp) + R (Share))) <= Epsilon * (R (Amp) + R (Share)) + Eta);
      Lemma_Amplification_Gain (R (Amp), R (Share), R (Amp + Share));
   end Lemma_Share;

   procedure Lemma_Above_Floor (Value : Dimensionless)
   with
     Ghost,
     Pre  => Value >= Smallest_Partial_Product,
     Post => R (Value) >= 2.0 * Normal_Floor and then abs (R (Value)) >= 2.0 * Normal_Floor;
   --  A running product within its limits is far above underflow.

   procedure Lemma_Above_Floor (Value : Dimensionless) is null;

   procedure Lemma_Q_Update (Q, F : Dimensionless; Before, Factor, A, S, An : Real)
   with
     Ghost,
     Pre  =>
       Q > 0.0
       and then Q <= Largest_Partial_Product
       and then abs F <= 2.0**452
       and then Q * F >= Smallest_Partial_Product
       and then A >= 16.0
       and then A <= Amplification_Bound
       and then S >= 1.0
       and then S <= Share_Bound
       and then An - 16.0 >= (A - 16.0) + (1.0 - Gain_Loss) * S
       and then abs (R (Q) - Before) <= Lambda * (A - 16.0) * abs Before
       and then abs (R (F) - Factor) <= Factor_Error * S * abs Factor,
     Post => abs (R (Q * F) - Before * Factor) <= Lambda * (An - 16.0) * abs (Before * Factor);
   --  The rounded product of the running product and a computed factor.

   procedure Lemma_Q_Update (Q, F : Dimensionless; Before, Factor, A, S, An : Real) is
   begin
      Float_Lemmas.Lemma_Rounding_Error_Mul (Q, F);
      Lemma_Above_Floor (Q * F);
      Lemma_Q_Step (R (Q), Before, R (F), Factor, R (Q * F), A, S, An);
   end Lemma_Q_Update;

   procedure Lemma_Q_Transfer (Qn, Before, Factor, Exact, After, Bound : Real)
   with
     Ghost,
     Pre  =>
       Exact = Factor
       and then After = Before * Exact
       and then abs (Qn - Before * Factor) <= Bound * abs (Before * Factor),
     Post => abs (Qn - After) <= Bound * abs After;
   --  The same, with the exact factor and the exact product under their own names.

   procedure Lemma_Q_Transfer (Qn, Before, Factor, Exact, After, Bound : Real) is
      pragma Unreferenced (Qn, Exact, Bound);
   begin
      pragma Assert (Before * Factor = After);
   end Lemma_Q_Transfer;

   ---------------------------------------------------------------------------
   --  The factor of a real pole
   ---------------------------------------------------------------------------

   procedure Lemma_Real_Factor_Error (Uh, Ue, P, Xh, Rc, Rn, Rt, HW, G, F, Mr : Real)
   with
     Ghost,
     Pre  =>
       abs (Uh - Ue) <= Distance_Error
       and then P /= 0.0
       and then abs P <= Big
       and then abs (Uh - P) >= Reach
       and then abs (Xh - (Uh - P)) <= Unit * abs (Uh - P)
       and then abs (Rc - 1.0 / (Uh - P)) <= Reciprocal_Error * abs (1.0 / (Uh - P))
       and then abs Rc <= Mr
       and then Mr >= 1.0
       and then Mr <= Share_Bound
       and then abs (Rn - 1.0 / P) <= 2.0 * Epsilon * abs (1.0 / P)
       and then HW >= Smallest_Ratio
       and then abs (Rt - HW) <= Ratio_Error * HW
       and then abs (G - (-Xh) * Rn) <= Epsilon * abs ((-Xh) * Rn) + Eta
       and then abs (F - Rt * G) <= Epsilon * abs (Rt * G) + Eta,
     Post => abs (F - HW * ((P - Ue) * (1.0 / P))) <= Factor_Error * Mr * abs (HW * ((P - Ue) * (1.0 / P)));
   --  The computed factor fl(Ratio·fl(-X·(1/p))) against (h/W)·(p - U)/p. Moving from the computed distance to
   --  the exact one costs their difference over the distance from the pole, which the reciprocal bounds.

   procedure Lemma_Real_Factor_Error (Uh, Ue, P, Xh, Rc, Rn, Rt, HW, G, F, Mr : Real) is
      Zh : constant Real := Uh - P;
      Ze : constant Real := Ue - P;
      W  : constant Real := Mr * abs Zh;
      E1 : constant Real := 5.001 * Epsilon * Mr;
      E2 : constant Real := 5.002 * Epsilon * Mr;
      Ex : constant Real := 6.01 * Epsilon * Mr;
      Eg : constant Real := 8.02 * Epsilon * Mr;
      Eh : constant Real := 9.03 * Epsilon * Mr;
      Ef : constant Real := 13.14 * Epsilon * Mr;
      GE : constant Real := (P - Ue) * (1.0 / P);
   begin
      --  The distance from the pole is at least about the reciprocal of the computed reciprocal.
      Lemma_Reciprocal (Zh);
      pragma Assert (Rc * Zh - 1.0 = (Rc - 1.0 / Zh) * Zh);
      Lemma_Product_Bound (Rc - 1.0 / Zh, Zh, Reciprocal_Error * abs (1.0 / Zh), abs Zh);
      pragma Assert ((Reciprocal_Error * abs (1.0 / Zh)) * abs Zh = Reciprocal_Error * (abs (1.0 / Zh) * abs Zh));
      pragma Assert (abs (Rc * Zh - 1.0) <= Reciprocal_Error);
      pragma Assert (abs (Rc * Zh) = abs Rc * abs Zh);
      pragma Assert (abs Rc * abs Zh >= 1.0 - Reciprocal_Error);
      Lemma_Scale (abs Rc, Mr, abs Zh);
      pragma Assert (W >= 1.0 - Reciprocal_Error);
      pragma Assert (Distance_Error <= 5.001 * Epsilon * W);
      pragma Assert (5.001 * Epsilon * W = E1 * abs Zh);
      pragma Assert (Zh - Ze = Uh - Ue);
      pragma Assert (abs (Zh - Ze) <= E1 * abs Zh);
      --  Relative to the exact distance from the pole instead.
      pragma Assert (E1 >= 0.0 and then E1 <= 0.0001);
      Lemma_Scale (0.9999, 1.0 - E1, E2);
      pragma Assert (E2 * 0.9999 >= E1);
      Lemma_Relative_Swap (Zh, Ze, E1, E2);
      --  The rounded difference, and the reciprocal of the pole.
      pragma Assert (Ex >= Unit + E2 + Unit * E2);
      Lemma_Relative_Chain (Xh, Zh, Ze, Unit, E2, Ex);
      pragma Assert (abs ((-Xh) - (P - Ue)) <= Ex * abs (P - Ue));
      pragma Assert (Eg >= Ex + 2.0 * Epsilon + Ex * (2.0 * Epsilon));
      Lemma_Relative_Product (-Xh, P - Ue, Rn, 1.0 / P, Ex, 2.0 * Epsilon, Eg);
      pragma Assert (abs ((-Xh) * Rn - GE) <= Eg * abs GE);
      --  Their rounded product is far above underflow.
      pragma Assert (abs Xh >= (1.0 - Unit) * Reach);
      Lemma_Reciprocal_Floor (P, Big);
      pragma Assert (abs (1.0 / P) >= 2.0 * Smallest_Reciprocal);
      pragma Assert (abs Rn >= Smallest_Reciprocal);
      Lemma_Product_Floor (-Xh, Rn, 0.5 * Reach, Smallest_Reciprocal);
      pragma Assert (0.5 * Reach * Smallest_Reciprocal >= 4.0 * Normal_Floor);
      Lemma_Normal_Rounding (G, (-Xh) * Rn);
      pragma Assert (Eh >= Unit + Eg + Unit * Eg);
      Lemma_Relative_Chain (G, (-Xh) * Rn, GE, Unit, Eg, Eh);
      --  The ratio h/W.
      pragma Assert (abs (Rt - HW) <= Ratio_Error * abs HW);
      pragma Assert (Ef >= Ratio_Error + Eh + Ratio_Error * Eh);
      Lemma_Relative_Product (Rt, HW, G, GE, Ratio_Error, Eh, Ef);
      pragma Assert (abs Rt >= 0.5 * Smallest_Ratio);
      pragma Assert (abs G >= (1.0 - Unit) * abs ((-Xh) * Rn));
      pragma Assert (abs G >= 0.25 * Reach * Smallest_Reciprocal);
      Lemma_Product_Floor (Rt, G, 0.5 * Smallest_Ratio, 0.25 * Reach * Smallest_Reciprocal);
      pragma Assert (0.5 * Smallest_Ratio * (0.25 * Reach * Smallest_Reciprocal) >= Normal_Floor);
      Lemma_Normal_Rounding (F, Rt * G);
      pragma Assert (Factor_Error * Mr >= Unit + Ef + Unit * Ef);
      Lemma_Relative_Chain (F, Rt * G, HW * GE, Unit, Ef, Factor_Error * Mr);
   end Lemma_Real_Factor_Error;

   procedure Lemma_Real_Factor_Exact
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; V : Real)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then
         (R (Cache.Pole_Slots (Slot).Pole_Component) < 0.0 or else R (Cache.Pole_Slots (Slot).Pole_Component) > 1.0)
       and then Model.Warp_Scale (Warp, V) >= Smallest_Scale
       and then Model.Distance (Warp, V) = V / Model.Warp_Scale (Warp, V),
     Post =>
       Model.Real_Factor (Cache, Slot, Warp, V)
       = (Model.Warp_Scale (Warp, V) / R (Warp))
         * ((R (Cache.Pole_Slots (Slot).Pole_Component) - Model.Distance (Warp, V))
            * (1.0 / R (Cache.Pole_Slots (Slot).Pole_Component)));
   --  The factor of a real pole, as the node loop computes it: (h/W)·(p - U)·(1/p).

   procedure Lemma_Real_Factor_Exact
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; V : Real)
   is
      P : constant Real := R (Cache.Pole_Slots (Slot).Pole_Component);
      W : constant Real := R (Warp);
      H : constant Real := Model.Warp_Scale (Warp, V);
   begin
      pragma Assert (W >= Smallest_Scale);
      Lemma_Real_Factor_Form (P, W, H, V);
      pragma Assert (Model.Real_Factor (Cache, Slot, Warp, V) = (P * H - V) / (P * W));
      Lemma_Division (P - V / H, P);
   end Lemma_Real_Factor_Exact;

   procedure Lemma_Real_Factor_Ranges (X, RN, Ratio, Q : Dimensionless)
   with
     Ghost,
     Pre  =>
       abs X <= 2.0**201
       and then abs RN <= Magnitude_Limit
       and then Ratio >= 2.0**(-53)
       and then Ratio <= 2.0**50
       and then Q > 0.0
       and then Q <= Largest_Partial_Product,
     Post =>
       abs (X * RN) <= 2.0**402
       and then abs (-X * RN) <= 2.0**402
       and then abs (Ratio * (-X * RN)) <= 2.0**452
       and then abs (Q * (Ratio * (-X * RN))) <= 2.0**800;
   --  Nothing overflows in the factor of a real pole.

   procedure Lemma_Real_Factor_Ranges (X, RN, Ratio, Q : Dimensionless) is
      pragma Unreferenced (Q);
   begin
      pragma Assert (abs (X * RN) <= 2.0**402);
      pragma Assert (abs (Ratio * (-X * RN)) <= 2.0**452);
   end Lemma_Real_Factor_Ranges;

   procedure Lemma_Real_Factor_Of_Term
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Term : Pole_Term; V : Real)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Poles.Is_Term_Bounded (Term)
       and then Poles.Is_Real_Term (Cache, Slot, Term)
       and then Model.Warp_Scale (Warp, V) >= Smallest_Scale
       and then Model.Distance (Warp, V) = V / Model.Warp_Scale (Warp, V),
     Post =>
       R (Term.Real_Part) /= 0.0
       and then abs (R (Term.Real_Part)) <= Big
       and then
         abs (R (Term.Reciprocal_Norm) - 1.0 / R (Term.Real_Part)) <= 2.0 * Epsilon * abs (1.0 / R (Term.Real_Part))
       and then
         Model.Real_Factor (Cache, Slot, Warp, V)
         = (Model.Warp_Scale (Warp, V) / R (Warp))
           * ((R (Term.Real_Part) - Model.Distance (Warp, V)) * (1.0 / R (Term.Real_Part)));
   --  The same, for the gathered pole of the slot, with what the node loop relies on about its reciprocal.

   procedure Lemma_Real_Factor_Of_Term
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Term : Pole_Term; V : Real)
   is
      P : constant Real := R (Term.Real_Part);
   begin
      pragma Assert (P = R (Cache.Pole_Slots (Slot).Pole_Component));
      pragma Assert (P < 0.0 or else P > 1.0);
      pragma Assert (abs Term.Real_Part <= Magnitude_Limit);
      Lemma_Limit (Term.Real_Part);
      Lemma_Real_Factor_Exact (Warp, Cache, Slot, V);
   end Lemma_Real_Factor_Of_Term;

   procedure Lemma_Real_Factor_Value (X, Rec, RN, Ratio : Dimensionless; Uh, Ue, P, HW, Mr : Real)
   with
     Ghost,
     Pre  =>
       abs X >= 2.0**(-101)
       and then abs X <= 2.0**201
       and then abs RN <= Magnitude_Limit
       and then Ratio >= 2.0**(-53)
       and then Ratio <= 2.0**50
       and then abs (X * RN) <= 2.0**402
       and then abs (Uh - Ue) <= Distance_Error
       and then P /= 0.0
       and then abs P <= Big
       and then abs (Uh - P) >= Reach
       and then abs (R (X) - (Uh - P)) <= Unit * abs (Uh - P)
       and then abs (R (Rec) - 1.0 / (Uh - P)) <= Reciprocal_Error * abs (1.0 / (Uh - P))
       and then abs (R (Rec)) <= Mr
       and then Mr >= 1.0
       and then Mr <= Share_Bound
       and then abs (R (RN) - 1.0 / P) <= 2.0 * Epsilon * abs (1.0 / P)
       and then HW >= Smallest_Ratio
       and then abs (R (Ratio) - HW) <= Ratio_Error * HW,
     Post =>
       abs (R (Ratio * (-X * RN)) - HW * ((P - Ue) * (1.0 / P)))
       <= Factor_Error * Mr * abs (HW * ((P - Ue) * (1.0 / P)));
   --  The computed factor of a real pole, with its rounded operations.

   procedure Lemma_Real_Factor_Value (X, Rec, RN, Ratio : Dimensionless; Uh, Ue, P, HW, Mr : Real) is
      G : constant Dimensionless := -X * RN;
   begin
      Float_Lemmas.Lemma_Rounding_Error_Mul (X, RN);
      pragma Assert (R (G) = -R (X * RN));
      pragma Assert ((-R (X)) * R (RN) = -(R (X) * R (RN)));
      pragma Assert (abs (R (G) - (-R (X)) * R (RN)) <= Epsilon * abs ((-R (X)) * R (RN)) + Eta);
      Float_Lemmas.Lemma_Rounding_Error_Mul (Ratio, G);
      Lemma_Real_Factor_Error (Uh, Ue, P, R (X), R (Rec), R (RN), R (Ratio), HW, R (G), R (Ratio * G), Mr);
   end Lemma_Real_Factor_Value;

   procedure Lemma_Real_Factor_Accuracy
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Node              : Dimensionless;
      Index             : Rational_Degree_Slot;
      U, Ratio, X, Rec  : Dimensionless;
      Q, Amp            : Dimensionless)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then Poles.Is_Cache_Prepared (Cache, Prepared)
       and then Index <= Cache.Real_Pole_Count
       and then Ratio >= 2.0**(-53)
       and then Ratio <= 2.0**50
       and then abs X >= 2.0**(-101)
       and then abs X <= 2.0**201
       and then abs Rec <= 2.0**101
       and then abs (R (U) - R (Prepared.Terms (Index).Real_Part)) >= Reach
       and then
         abs (R (X) - (R (U) - R (Prepared.Terms (Index).Real_Part)))
         <= Unit * abs (R (U) - R (Prepared.Terms (Index).Real_Part))
       and then
         abs (R (Rec) - 1.0 / (R (U) - R (Prepared.Terms (Index).Real_Part)))
         <= Reciprocal_Error * abs (1.0 / (R (U) - R (Prepared.Terms (Index).Real_Part)))
       and then Q > 0.0
       and then Q <= Largest_Partial_Product
       and then Amp >= 16.0
       and then Amp <= 2.0**206
       and then Is_Setup_Accurate (Warp, R (Node), U, Ratio)
       and then Is_Q_State (Warp, Cache, Denominator_Scale, R (Node), True, Index - 1, Q, Amp)
       and then abs (X * Prepared.Terms (Index).Reciprocal_Norm) <= 2.0**402
       and then abs (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm)) <= 2.0**452
       and then abs (Q * (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm))) <= 2.0**800
       and then Q * (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm)) >= Smallest_Partial_Product
       and then Dimensionless'Max (1.0, abs Rec) >= 1.0
       and then Dimensionless'Max (1.0, abs Rec) <= 2.0**201
       and then Amp + Dimensionless'Max (1.0, abs Rec) >= Amp
       and then Amp + Dimensionless'Max (1.0, abs Rec) <= Largest_Amplification,
     Post =>
       Is_Q_State
         (Warp,
          Cache,
          Denominator_Scale,
          R (Node),
          True,
          Index,
          Q * (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm)),
          Amp + Dimensionless'Max (1.0, abs Rec));
   --  The running product after the factor of a real pole, at an accurate sample.

   procedure Lemma_Real_Factor_Accuracy
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Node              : Dimensionless;
      Index             : Rational_Degree_Slot;
      U, Ratio, X, Rec  : Dimensionless;
      Q, Amp            : Dimensionless)
   is
      Term   : constant Pole_Term := Prepared.Terms (Index);
      Share  : constant Dimensionless := Dimensionless'Max (1.0, abs Rec);
      F      : constant Dimensionless := Ratio * (-X * Term.Reciprocal_Norm);
      V      : constant Real := R (Node);
      Ue     : constant Real := Model.Distance (Warp, V);
      HW     : constant Real := Model.Warp_Scale (Warp, V) / R (Warp);
      P      : constant Real := R (Term.Real_Part);
      Before : constant Real := Partial_Denominator (Cache, Denominator_Scale, Warp, Index - 1, V);
   begin
      Lemma_Term_Kind (Cache, Prepared, Index);
      Lemma_Q_State_Elim (Warp, Cache, Denominator_Scale, V, Index - 1, Q, Amp);
      Lemma_Setup_Elim (Warp, V, U, Ratio);
      Lemma_Real_Factor_Of_Term (Warp, Cache, Index, Term, V);
      Lemma_Partial_Real_Step (Cache, Denominator_Scale, Warp, Index, V);
      Lemma_Abs (Rec);
      Lemma_Maximum (abs Rec);
      Lemma_Share (Amp, Share);
      Lemma_Real_Factor_Value (X, Rec, Term.Reciprocal_Norm, Ratio, R (U), Ue, P, HW, R (Share));
      Lemma_Q_Update (Q, F, Before, HW * ((P - Ue) * (1.0 / P)), R (Amp), R (Share), R (Amp + Share));
      Lemma_Q_Transfer
        (R (Q * F),
         Before,
         HW * ((P - Ue) * (1.0 / P)),
         Model.Real_Factor (Cache, Index, Warp, V),
         Partial_Denominator (Cache, Denominator_Scale, Warp, Index, V),
         Lambda * (R (Amp + Share) - 16.0));
      Lemma_Q_State_Intro (Warp, Cache, Denominator_Scale, V, True, Index, Q * F, Amp + Share);
   end Lemma_Real_Factor_Accuracy;

   procedure Lemma_Real_Factor
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Node              : Dimensionless;
      Accurate          : Boolean;
      Index             : Rational_Degree_Slot;
      U, Ratio, X, Rec  : Dimensionless;
      Q, Amp            : Dimensionless)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then Poles.Is_Cache_Prepared (Cache, Prepared)
       and then Index <= Cache.Real_Pole_Count
       and then Ratio >= 2.0**(-53)
       and then Ratio <= 2.0**50
       and then abs X >= 2.0**(-101)
       and then abs X <= 2.0**201
       and then abs Rec <= 2.0**101
       and then abs (R (U) - R (Prepared.Terms (Index).Real_Part)) >= Reach
       and then
         abs (R (X) - (R (U) - R (Prepared.Terms (Index).Real_Part)))
         <= Unit * abs (R (U) - R (Prepared.Terms (Index).Real_Part))
       and then
         abs (R (Rec) - 1.0 / (R (U) - R (Prepared.Terms (Index).Real_Part)))
         <= Reciprocal_Error * abs (1.0 / (R (U) - R (Prepared.Terms (Index).Real_Part)))
       and then Q > 0.0
       and then Q <= Largest_Partial_Product
       and then Amp >= 16.0
       and then R (Amp) <= 16.0 + To_Real (Index - 1) * Amplification_Step
       and then (if Accurate then Is_Setup_Accurate (Warp, R (Node), U, Ratio))
       and then Is_Q_State (Warp, Cache, Denominator_Scale, R (Node), Accurate, Index - 1, Q, Amp),
     Post =>
       abs (X * Prepared.Terms (Index).Reciprocal_Norm) <= 2.0**402
       and then abs (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm)) <= 2.0**452
       and then abs (Q * (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm))) <= 2.0**800
       and then Amp <= 2.0**206
       and then Dimensionless'Max (1.0, abs Rec) <= 2.0**201
       and then Amp + Dimensionless'Max (1.0, abs Rec) >= 16.0
       and then R (Amp + Dimensionless'Max (1.0, abs Rec)) <= 16.0 + To_Real (Index) * Amplification_Step
       and then
         (if Q * (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm)) >= Smallest_Partial_Product
          then
            Is_Q_State
              (Warp,
               Cache,
               Denominator_Scale,
               R (Node),
               Accurate,
               Index,
               Q * (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm)),
               Amp + Dimensionless'Max (1.0, abs Rec)));
   --  The factor of a real pole: nothing overflows, and the running product stays accurate unless it falls
   --  below its lower limit.

   procedure Lemma_Real_Factor
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Node              : Dimensionless;
      Accurate          : Boolean;
      Index             : Rational_Degree_Slot;
      U, Ratio, X, Rec  : Dimensionless;
      Q, Amp            : Dimensionless)
   is
      RN : constant Dimensionless := Prepared.Terms (Index).Reciprocal_Norm;
   begin
      Lemma_Term_Kind (Cache, Prepared, Index);
      Lemma_Real_Factor_Ranges (X, RN, Ratio, Q);
      Lemma_Abs (Rec);
      Lemma_Maximum (abs Rec);
      pragma Assert (abs (R (Rec)) <= Inverse_Limit);
      pragma Assert (R (Dimensionless'Max (1.0, abs Rec)) <= Inverse_Limit);
      Lemma_Amplification (Amp, Dimensionless'Max (1.0, abs Rec), Index);
      if Accurate and then Amp + Dimensionless'Max (1.0, abs Rec) <= Largest_Amplification then
         if Q * (Ratio * (-X * RN)) >= Smallest_Partial_Product then
            Lemma_Real_Factor_Accuracy
              (Warp, Cache, Denominator_Scale, Prepared, Node, Index, U, Ratio, X, Rec, Q, Amp);
         end if;
      else
         Lemma_Q_State_Intro
           (Warp,
            Cache,
            Denominator_Scale,
            R (Node),
            Accurate,
            Index,
            Q * (Ratio * (-X * RN)),
            Amp + Dimensionless'Max (1.0, abs Rec));
      end if;
   end Lemma_Real_Factor;

   ---------------------------------------------------------------------------
   --  The factor of a pair
   ---------------------------------------------------------------------------

   Norm_Limit : constant := 2.0**401;
   --  No squared magnitude of a pole is larger.

   Smallest_Norm_Reciprocal : constant := 2.0**(-402);
   --  Less than the computed reciprocal of any of them.

   procedure Lemma_Pair_Factor_Range (Z, A, B, Sq, Rn : Real)
   with
     Ghost,
     Pre  =>
       abs Z <= 1.0 + abs A
       and then A * A + B * B > 0.0
       and then abs (Sq - (Z * Z + B * B)) <= Square_Error * (Z * Z + B * B)
       and then abs (Rn - 1.0 / (A * A + B * B)) <= 8.0 * Epsilon * (1.0 / (A * A + B * B))
       and then abs Rn <= Big,
     Post => Rn > 0.0 and then Sq >= 0.0 and then abs (Sq * Rn) <= 4.0 * Big;
   --  |U - π|²/|π|² ≤ 2 + 2/|π|² for U in [0, 1]: the computed factor of a pair is bounded through the
   --  reciprocal of |π|², although neither |U - π|² nor that reciprocal is small by itself.

   procedure Lemma_Pair_Factor_Range (Z, A, B, Sq, Rn : Real) is
      N         : constant Real := A * A + B * B;
      Inverse_N : constant Real := 1.0 / N;
      D         : constant Real := Z * Z + B * B;
   begin
      Lemma_Reciprocal (N);
      pragma Assert (Inverse_N > 0.0);
      pragma Assert (Rn >= (1.0 - 8.0 * Epsilon) * Inverse_N);
      pragma Assert (Rn > 0.0);
      pragma Assert (Z * Z >= 0.0 and then B * B >= 0.0 and then A * A >= 0.0);
      pragma Assert (D >= 0.0);
      pragma Assert (Sq >= (1.0 - Square_Error) * D);
      pragma Assert (Sq >= 0.0);
      Lemma_Product_Bound (Z, Z, 1.0 + abs A, 1.0 + abs A);
      pragma Assert (abs (Z * Z) = Z * Z);
      pragma Assert ((1.0 - abs A) * (1.0 - abs A) >= 0.0);
      pragma Assert (abs A * abs A = A * A);
      pragma Assert ((1.0 + abs A) * (1.0 + abs A) <= 2.0 + 2.0 * (A * A));
      pragma Assert (D <= 2.0 + 2.0 * N);
      pragma Assert (Sq <= (1.0 + Square_Error) * (2.0 + 2.0 * N));
      Lemma_Scale (Sq, (1.0 + Square_Error) * (2.0 + 2.0 * N), Rn);
      pragma Assert (N * Rn <= (1.0 + 8.0 * Epsilon) * (N * Inverse_N));
      pragma Assert (N * Rn <= 1.0 + 8.0 * Epsilon);
      pragma
        Assert (((1.0 + Square_Error) * (2.0 + 2.0 * N)) * Rn = (1.0 + Square_Error) * (2.0 * Rn + 2.0 * (N * Rn)));
      pragma Assert (Sq * Rn <= (1.0 + Square_Error) * (2.0 * Big + 2.0 * (1.0 + 8.0 * Epsilon)));
      pragma Assert (Sq * Rn >= 0.0);
   end Lemma_Pair_Factor_Range;

   procedure Lemma_Pair_Distance_Shift (Uh, Ue, A, B, Iv, Mr : Real)
   with
     Ghost,
     Pre  =>
       abs (Uh - Ue) <= Distance_Error
       and then (Uh - A) * (Uh - A) + B * B >= Smallest_Square
       and then abs (Iv - 1.0 / ((Uh - A) * (Uh - A) + B * B)) <= Inverse_Error * (1.0 / ((Uh - A) * (Uh - A) + B * B))
       and then Iv <= Mr
       and then Mr >= 1.0
       and then Mr <= Share_Bound,
     Post =>
       abs (((Uh - A) * (Uh - A) + B * B) - ((Ue - A) * (Ue - A) + B * B))
       <= 10.02 * Epsilon * Mr * abs ((Ue - A) * (Ue - A) + B * B);
   --  |U - π|² at the computed distance against the exact one: they differ by the error of the distance times
   --  2·|U - A| ≤ |U - π|² + 1, which is at most twice the larger of one and its reciprocal times |U - π|².

   procedure Lemma_Pair_Distance_Shift (Uh, Ue, A, B, Iv, Mr : Real) is
      Zh : constant Real := Uh - A;
      Dl : constant Real := Uh - Ue;
      Dh : constant Real := Zh * Zh + B * B;
      De : constant Real := (Ue - A) * (Ue - A) + B * B;
      RD : constant Real := 1.0 / Dh;
      W  : constant Real := Mr * Dh;
      E1 : constant Real := 10.01 * Epsilon * Mr;
      E2 : constant Real := 10.02 * Epsilon * Mr;
   begin
      pragma Assert (Ue - A = Zh - Dl);
      pragma Assert (Dh - De = Dl * (2.0 * Zh - Dl));
      Lemma_Product_Bound (Dl, 2.0 * Zh - Dl, Distance_Error, 2.0 * abs Zh + Distance_Error);
      pragma Assert ((abs Zh - 1.0) * (abs Zh - 1.0) >= 0.0);
      pragma Assert (abs Zh * abs Zh = Zh * Zh);
      pragma Assert (B * B >= 0.0);
      pragma Assert (2.0 * abs Zh <= Dh + 1.0);
      pragma Assert (abs (Dh - De) <= Distance_Error * (Dh + 1.001));
      --  One is at most about the reciprocal times the squared distance.
      Lemma_Reciprocal (Dh);
      pragma Assert (RD > 0.0);
      pragma Assert (Iv * Dh - 1.0 = (Iv - RD) * Dh);
      Lemma_Product_Bound (Iv - RD, Dh, Inverse_Error * RD, Dh);
      pragma Assert ((Inverse_Error * RD) * Dh = Inverse_Error * (RD * Dh));
      pragma Assert (Iv * Dh >= 1.0 - Inverse_Error);
      Lemma_Scale (Iv, Mr, Dh);
      pragma Assert (W >= 1.0 - Inverse_Error);
      Lemma_Scale (1.0, Mr, Dh);
      pragma Assert (Dh <= W);
      pragma Assert (Dh + 1.001 <= 2.002 * W);
      pragma Assert (Distance_Error * (Dh + 1.001) <= 10.01 * Epsilon * W);
      pragma Assert (10.01 * Epsilon * W = E1 * Dh);
      pragma Assert (abs Dh = Dh);
      pragma Assert (abs (Dh - De) <= E1 * abs Dh);
      pragma Assert (E1 >= 0.0 and then E1 <= 0.0001);
      Lemma_Scale (0.9999, 1.0 - E1, E2);
      pragma Assert (E2 * 0.9999 >= E1);
      Lemma_Relative_Swap (Dh, De, E1, E2);
   end Lemma_Pair_Distance_Shift;

   procedure Lemma_Pair_Factor_Error (Dh, De, N, Sq, Rn, Rt, HW, RR, SN, F, Mr : Real)
   with
     Ghost,
     Pre  =>
       Dh >= Smallest_Square
       and then abs (Dh - De) <= 10.02 * Epsilon * Mr * abs De
       and then Mr >= 1.0
       and then Mr <= Share_Bound
       and then abs (Sq - Dh) <= Square_Error * Dh
       and then N > 0.0
       and then N <= Norm_Limit
       and then abs (Rn - 1.0 / N) <= 8.0 * Epsilon * (1.0 / N)
       and then HW >= Smallest_Ratio
       and then abs (Rt - HW) <= Ratio_Error * HW
       and then abs (RR - Rt * Rt) <= Epsilon * abs (Rt * Rt) + Eta
       and then abs (SN - Sq * Rn) <= Epsilon * abs (Sq * Rn) + Eta
       and then abs (F - RR * SN) <= Epsilon * abs (RR * SN) + Eta,
     Post => abs (F - (HW * HW) * (De * (1.0 / N))) <= Factor_Error * Mr * abs ((HW * HW) * (De * (1.0 / N)));
   --  The computed factor fl(fl(Ratio²)·fl(Sq·(1/|π|²))) against (h/W)²·|U - π|²/|π|².

   procedure Lemma_Pair_Factor_Error (Dh, De, N, Sq, Rn, Rt, HW, RR, SN, F, Mr : Real) is
      E2        : constant Real := 10.02 * Epsilon * Mr;
      Es        : constant Real := 14.13 * Epsilon * Mr;
      En        : constant Real := 22.14 * Epsilon * Mr;
      Em        : constant Real := 23.15 * Epsilon * Mr;
      Ep        : constant Real := 32.38 * Epsilon * Mr;
      Inverse_N : constant Real := 1.0 / N;
   begin
      Lemma_Reciprocal (N);
      pragma Assert (Inverse_N > 0.0);
      --  The squared distance.
      pragma Assert (abs (Sq - Dh) <= Square_Error * abs Dh);
      pragma Assert (Es >= Square_Error + E2 + Square_Error * E2);
      Lemma_Relative_Chain (Sq, Dh, De, Square_Error, E2, Es);
      --  Times the reciprocal of |π|².
      pragma Assert (abs (Rn - Inverse_N) <= 8.0 * Epsilon * abs Inverse_N);
      pragma Assert (En >= Es + 8.0 * Epsilon + Es * (8.0 * Epsilon));
      Lemma_Relative_Product (Sq, De, Rn, Inverse_N, Es, 8.0 * Epsilon, En);
      pragma Assert (abs Sq >= 0.5 * Smallest_Square);
      Lemma_Reciprocal_Floor (N, Norm_Limit);
      pragma Assert (Inverse_N >= 2.0 * Smallest_Norm_Reciprocal);
      pragma Assert (abs Rn >= Smallest_Norm_Reciprocal);
      Lemma_Product_Floor (Sq, Rn, 0.5 * Smallest_Square, Smallest_Norm_Reciprocal);
      pragma Assert (0.5 * Smallest_Square * Smallest_Norm_Reciprocal >= 4.0 * Normal_Floor);
      Lemma_Normal_Rounding (SN, Sq * Rn);
      pragma Assert (Em >= Unit + En + Unit * En);
      Lemma_Relative_Chain (SN, Sq * Rn, De * Inverse_N, Unit, En, Em);
      --  The squared ratio.
      pragma Assert (abs (Rt - HW) <= Ratio_Error * abs HW);
      Lemma_Relative_Product (Rt, HW, Rt, HW, Ratio_Error, Ratio_Error, 8.21 * Epsilon);
      pragma Assert (abs Rt >= 0.5 * Smallest_Ratio);
      Lemma_Product_Floor (Rt, Rt, 0.5 * Smallest_Ratio, 0.5 * Smallest_Ratio);
      pragma Assert (0.5 * Smallest_Ratio * (0.5 * Smallest_Ratio) >= 4.0 * Normal_Floor);
      Lemma_Normal_Rounding (RR, Rt * Rt);
      Lemma_Relative_Chain (RR, Rt * Rt, HW * HW, Unit, 8.21 * Epsilon, 9.22 * Epsilon);
      --  Their product.
      pragma Assert (Ep >= 9.22 * Epsilon + Em + 9.22 * Epsilon * Em);
      Lemma_Relative_Product (RR, HW * HW, SN, De * Inverse_N, 9.22 * Epsilon, Em, Ep);
      pragma Assert (abs RR >= (1.0 - Unit) * abs (Rt * Rt));
      pragma Assert (abs RR >= 0.125 * Smallest_Ratio * Smallest_Ratio);
      pragma Assert (abs SN >= (1.0 - Unit) * abs (Sq * Rn));
      pragma Assert (abs SN >= 0.25 * Smallest_Square * Smallest_Norm_Reciprocal);
      Lemma_Product_Floor
        (RR, SN, 0.125 * Smallest_Ratio * Smallest_Ratio, 0.25 * Smallest_Square * Smallest_Norm_Reciprocal);
      pragma
        Assert
          (0.125 * Smallest_Ratio * Smallest_Ratio * (0.25 * Smallest_Square * Smallest_Norm_Reciprocal)
           >= Normal_Floor);
      Lemma_Normal_Rounding (F, RR * SN);
      pragma Assert (Factor_Error * Mr >= Unit + Ep + Unit * Ep);
      Lemma_Relative_Chain (F, RR * SN, (HW * HW) * (De * Inverse_N), Unit, Ep, Factor_Error * Mr);
   end Lemma_Pair_Factor_Error;

   procedure Lemma_Pair_Factor_Exact
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; V : Real)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then Pair <= Cache.Pair_Count
       and then R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) > 0.0
       and then Model.Warp_Scale (Warp, V) >= Smallest_Scale
       and then Model.Distance (Warp, V) = V / Model.Warp_Scale (Warp, V),
     Post =>
       Model.Pair_Norm (Cache, Pair) > 0.0
       and then
         Model.Pair_Factor (Cache, Pair, Warp, V)
         = ((Model.Warp_Scale (Warp, V) / R (Warp)) * (Model.Warp_Scale (Warp, V) / R (Warp)))
           * (((Model.Distance (Warp, V) - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component))
               * (Model.Distance (Warp, V) - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component))
               + R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component)
                 * R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component))
              * (1.0 / Model.Pair_Norm (Cache, Pair)));
   --  The factor of a pair, as the node loop computes it: (h/W)²·|U - π|²·(1/|π|²).

   procedure Lemma_Pair_Factor_Exact
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; V : Real)
   is
      A : constant Real := R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component);
      B : constant Real := R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component);
      W : constant Real := R (Warp);
      H : constant Real := Model.Warp_Scale (Warp, V);
      N : constant Real := Model.Pair_Norm (Cache, Pair);
   begin
      pragma Assert (W >= Smallest_Scale);
      pragma Assert (B * B > 0.0);
      pragma Assert (A * A >= 0.0);
      pragma Assert (N = A * A + B * B);
      pragma Assert (N > 0.0);
      Lemma_Pair_Factor_Form (A, B, W, H, V);
      pragma Assert (N * (W * W) /= 0.0);
      pragma
        Assert
          (Model.Pair_Factor (Cache, Pair, Warp, V) = ((V - A * H) * (V - A * H) + (B * H) * (B * H)) / (N * (W * W)));
      Lemma_Division ((V / H - A) * (V / H - A) + B * B, N);
   end Lemma_Pair_Factor_Exact;

   procedure Lemma_Pair_Factor_Ranges (Ratio, Sq, RN, Q : Dimensionless)
   with
     Ghost,
     Pre  =>
       Ratio >= 2.0**(-53)
       and then Ratio <= 2.0**50
       and then Sq >= 0.0
       and then Sq <= 2.0**403
       and then abs RN <= Magnitude_Limit
       and then abs (Sq * RN) <= 2.0**203
       and then Q > 0.0
       and then Q <= Largest_Partial_Product,
     Post =>
       abs (Ratio * Ratio) <= 2.0**101
       and then abs ((Ratio * Ratio) * (Sq * RN)) <= 2.0**304
       and then abs (Q * ((Ratio * Ratio) * (Sq * RN))) <= 2.0**640;
   --  Nothing overflows in the factor of a pair.

   procedure Lemma_Pair_Factor_Ranges (Ratio, Sq, RN, Q : Dimensionless) is
      pragma Unreferenced (Q);
   begin
      pragma Assert (abs (Ratio * Ratio) <= 2.0**101);
      pragma Assert (abs ((Ratio * Ratio) * (Sq * RN)) <= 2.0**304);
   end Lemma_Pair_Factor_Ranges;

   Product_Limit : constant := 2.0**203;
   --  More than the product of a computed squared distance and the computed reciprocal of |π|².

   procedure Lemma_Within_Product_Limit (Value : Dimensionless)
   with Ghost, Pre => abs (R (Value)) <= Product_Limit, Post => abs Value <= 2.0**203;

   procedure Lemma_Within_Product_Limit (Value : Dimensionless) is null;

   procedure Lemma_Pair_Product_Limit (Sq, RN : Dimensionless)
   with
     Ghost,
     Pre  => abs Sq <= 2.0**403 and then abs RN <= Magnitude_Limit and then abs (R (Sq) * R (RN)) <= 4.0 * Big,
     Post => abs (Sq * RN) <= 2.0**203;
   --  The rounded product within its limit, from the exact one.

   procedure Lemma_Pair_Product_Limit (Sq, RN : Dimensionless) is
   begin
      Float_Lemmas.Lemma_Rounding_Error_Mul (Sq, RN);
      pragma Assert (abs (R (Sq * RN)) <= (1.0 + Epsilon) * (4.0 * Big) + Eta);
      pragma Assert (abs (R (Sq * RN)) <= Product_Limit);
      Lemma_Within_Product_Limit (Sq * RN);
   end Lemma_Pair_Product_Limit;

   procedure Lemma_Pair_Factor_Of_Term
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term; V : Real)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then Pair <= Cache.Pair_Count
       and then Poles.Is_Term_Bounded (Term)
       and then Poles.Is_Pair_Term (Cache, Pair, Term)
       and then Model.Warp_Scale (Warp, V) >= Smallest_Scale
       and then Model.Distance (Warp, V) = V / Model.Warp_Scale (Warp, V),
     Post =>
       R (Term.Real_Part) * R (Term.Real_Part) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part) > 0.0
       and then
         R (Term.Real_Part) * R (Term.Real_Part) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part) <= Norm_Limit
       and then
         abs (R (Term.Reciprocal_Norm)
              - 1.0 / (R (Term.Real_Part) * R (Term.Real_Part) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
         <= 8.0
            * Epsilon
            * (1.0 / (R (Term.Real_Part) * R (Term.Real_Part) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
       and then
         Model.Pair_Factor (Cache, Pair, Warp, V)
         = ((Model.Warp_Scale (Warp, V) / R (Warp)) * (Model.Warp_Scale (Warp, V) / R (Warp)))
           * (((Model.Distance (Warp, V) - R (Term.Real_Part)) * (Model.Distance (Warp, V) - R (Term.Real_Part))
               + R (Term.Imaginary_Part) * R (Term.Imaginary_Part))
              * (1.0 / (R (Term.Real_Part) * R (Term.Real_Part) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part))));
   --  The same, for the gathered pair, with what the node loop relies on about the reciprocal of |π|².

   procedure Lemma_Pair_Factor_Of_Term
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term; V : Real)
   is
      A : constant Real := R (Term.Real_Part);
      B : constant Real := R (Term.Imaginary_Part);
   begin
      pragma Assert (A = R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component));
      pragma Assert (B = R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component));
      pragma Assert (B > 0.0);
      pragma Assert (abs Term.Real_Part <= Magnitude_Limit);
      pragma Assert (abs Term.Imaginary_Part <= Magnitude_Limit);
      Lemma_Limit (Term.Real_Part);
      Lemma_Limit (Term.Imaginary_Part);
      Lemma_Product_Bound (A, A, Big, Big);
      Lemma_Product_Bound (B, B, Big, Big);
      pragma Assert (A * A >= 0.0 and then B * B > 0.0);
      pragma Assert (A * A + B * B <= Norm_Limit);
      Lemma_Pair_Factor_Exact (Warp, Cache, Pair, V);
      pragma Assert (Model.Pair_Norm (Cache, Pair) = A * A + B * B);
   end Lemma_Pair_Factor_Of_Term;

   procedure Lemma_Pair_Factor_Value (Ratio, Sq, RN : Dimensionless; Dh, De, N, HW, Mr : Real)
   with
     Ghost,
     Pre  =>
       Ratio >= 2.0**(-53)
       and then Ratio <= 2.0**50
       and then Sq >= 0.0
       and then Sq <= 2.0**403
       and then abs RN <= Magnitude_Limit
       and then abs (Sq * RN) <= 2.0**203
       and then abs (Ratio * Ratio) <= 2.0**101
       and then Dh >= Smallest_Square
       and then abs (Dh - De) <= 10.02 * Epsilon * Mr * abs De
       and then Mr >= 1.0
       and then Mr <= Share_Bound
       and then abs (R (Sq) - Dh) <= Square_Error * Dh
       and then N > 0.0
       and then N <= Norm_Limit
       and then abs (R (RN) - 1.0 / N) <= 8.0 * Epsilon * (1.0 / N)
       and then HW >= Smallest_Ratio
       and then abs (R (Ratio) - HW) <= Ratio_Error * HW,
     Post =>
       abs (R ((Ratio * Ratio) * (Sq * RN)) - (HW * HW) * (De * (1.0 / N)))
       <= Factor_Error * Mr * abs ((HW * HW) * (De * (1.0 / N)));
   --  The computed factor of a pair, with its rounded operations.

   procedure Lemma_Pair_Factor_Value (Ratio, Sq, RN : Dimensionless; Dh, De, N, HW, Mr : Real) is
   begin
      Float_Lemmas.Lemma_Rounding_Error_Mul (Ratio, Ratio);
      Float_Lemmas.Lemma_Rounding_Error_Mul (Sq, RN);
      Float_Lemmas.Lemma_Rounding_Error_Mul (Ratio * Ratio, Sq * RN);
      Lemma_Pair_Factor_Error
        (Dh,
         De,
         N,
         R (Sq),
         R (RN),
         R (Ratio),
         HW,
         R (Ratio * Ratio),
         R (Sq * RN),
         R ((Ratio * Ratio) * (Sq * RN)),
         Mr);
   end Lemma_Pair_Factor_Value;

   procedure Lemma_Pair_Factor_Accuracy
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Node              : Dimensionless;
      Index             : Rational_Degree_Slot;
      U, Ratio, Sq, Inv : Dimensionless;
      Q, Amp            : Dimensionless)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then Poles.Is_Cache_Prepared (Cache, Prepared)
       and then Index > Cache.Real_Pole_Count
       and then Index <= Prepared.Count
       and then Ratio >= 2.0**(-53)
       and then Ratio <= 2.0**50
       and then Sq >= 2.0**(-201)
       and then Sq <= 2.0**403
       and then Inv >= 0.0
       and then Inv <= 2.0**201
       and then
         (R (U) - R (Prepared.Terms (Index).Real_Part)) * (R (U) - R (Prepared.Terms (Index).Real_Part))
         + R (Prepared.Terms (Index).Imaginary_Part) * R (Prepared.Terms (Index).Imaginary_Part)
         >= Smallest_Square
       and then
         abs (R (Sq)
              - ((R (U) - R (Prepared.Terms (Index).Real_Part)) * (R (U) - R (Prepared.Terms (Index).Real_Part))
                 + R (Prepared.Terms (Index).Imaginary_Part) * R (Prepared.Terms (Index).Imaginary_Part)))
         <= Square_Error
            * ((R (U) - R (Prepared.Terms (Index).Real_Part)) * (R (U) - R (Prepared.Terms (Index).Real_Part))
               + R (Prepared.Terms (Index).Imaginary_Part) * R (Prepared.Terms (Index).Imaginary_Part))
       and then
         abs (R (Inv)
              - 1.0
                / ((R (U) - R (Prepared.Terms (Index).Real_Part)) * (R (U) - R (Prepared.Terms (Index).Real_Part))
                   + R (Prepared.Terms (Index).Imaginary_Part) * R (Prepared.Terms (Index).Imaginary_Part)))
         <= Inverse_Error
            * (1.0
               / ((R (U) - R (Prepared.Terms (Index).Real_Part)) * (R (U) - R (Prepared.Terms (Index).Real_Part))
                  + R (Prepared.Terms (Index).Imaginary_Part) * R (Prepared.Terms (Index).Imaginary_Part)))
       and then Q > 0.0
       and then Q <= Largest_Partial_Product
       and then Amp >= 16.0
       and then Amp <= 2.0**206
       and then Is_Setup_Accurate (Warp, R (Node), U, Ratio)
       and then Is_Q_State (Warp, Cache, Denominator_Scale, R (Node), True, Index - 1, Q, Amp)
       and then abs Prepared.Terms (Index).Reciprocal_Norm <= Magnitude_Limit
       and then abs (Sq * Prepared.Terms (Index).Reciprocal_Norm) <= 2.0**203
       and then abs (Ratio * Ratio) <= 2.0**101
       and then abs ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm)) <= 2.0**304
       and then abs (Q * ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm))) <= 2.0**640
       and then Q * ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm)) >= Smallest_Partial_Product
       and then Dimensionless'Max (1.0, Inv) >= 1.0
       and then Dimensionless'Max (1.0, Inv) <= 2.0**201
       and then Amp + Dimensionless'Max (1.0, Inv) >= Amp
       and then Amp + Dimensionless'Max (1.0, Inv) <= Largest_Amplification,
     Post =>
       Is_Q_State
         (Warp,
          Cache,
          Denominator_Scale,
          R (Node),
          True,
          Index,
          Q * ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm)),
          Amp + Dimensionless'Max (1.0, Inv));
   --  The running product after the factor of a pair, at an accurate sample.

   procedure Lemma_Pair_Factor_Accuracy
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Node              : Dimensionless;
      Index             : Rational_Degree_Slot;
      U, Ratio, Sq, Inv : Dimensionless;
      Q, Amp            : Dimensionless)
   is
      Pair   : constant Rational_Pair_Index := Index - Cache.Real_Pole_Count;
      Term   : constant Pole_Term := Prepared.Terms (Index);
      Share  : constant Dimensionless := Dimensionless'Max (1.0, Inv);
      F      : constant Dimensionless := (Ratio * Ratio) * (Sq * Term.Reciprocal_Norm);
      V      : constant Real := R (Node);
      Ue     : constant Real := Model.Distance (Warp, V);
      HW     : constant Real := Model.Warp_Scale (Warp, V) / R (Warp);
      A      : constant Real := R (Term.Real_Part);
      B      : constant Real := R (Term.Imaginary_Part);
      N      : constant Real := A * A + B * B;
      Dh     : constant Real := (R (U) - A) * (R (U) - A) + B * B;
      De     : constant Real := (Ue - A) * (Ue - A) + B * B;
      Before : constant Real := Partial_Denominator (Cache, Denominator_Scale, Warp, Index - 1, V);
   begin
      Lemma_Term_Kind (Cache, Prepared, Index);
      Lemma_Q_State_Elim (Warp, Cache, Denominator_Scale, V, Index - 1, Q, Amp);
      Lemma_Setup_Elim (Warp, V, U, Ratio);
      Lemma_Pair_Factor_Of_Term (Warp, Cache, Pair, Term, V);
      Lemma_Partial_Pair_Step (Cache, Denominator_Scale, Warp, Index, V);
      Lemma_Maximum (Inv);
      Lemma_Share (Amp, Share);
      Lemma_Pair_Distance_Shift (R (U), Ue, A, B, R (Inv), R (Share));
      Lemma_Pair_Factor_Value (Ratio, Sq, Term.Reciprocal_Norm, Dh, De, N, HW, R (Share));
      Lemma_Q_Update (Q, F, Before, (HW * HW) * (De * (1.0 / N)), R (Amp), R (Share), R (Amp + Share));
      Lemma_Q_Transfer
        (R (Q * F),
         Before,
         (HW * HW) * (De * (1.0 / N)),
         Model.Pair_Factor (Cache, Pair, Warp, V),
         Partial_Denominator (Cache, Denominator_Scale, Warp, Index, V),
         Lambda * (R (Amp + Share) - 16.0));
      Lemma_Q_State_Intro (Warp, Cache, Denominator_Scale, V, True, Index, Q * F, Amp + Share);
   end Lemma_Pair_Factor_Accuracy;

   procedure Lemma_Pair_Factor
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Node              : Dimensionless;
      Accurate          : Boolean;
      Index             : Rational_Degree_Slot;
      U, Ratio, Sq, Inv : Dimensionless;
      Q, Amp            : Dimensionless)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then Poles.Is_Cache_Prepared (Cache, Prepared)
       and then Index > Cache.Real_Pole_Count
       and then Index <= Prepared.Count
       and then U >= 0.0
       and then U <= 1.0
       and then Ratio >= 2.0**(-53)
       and then Ratio <= 2.0**50
       and then Sq >= 2.0**(-201)
       and then Sq <= 2.0**403
       and then Inv >= 0.0
       and then Inv <= 2.0**201
       and then R (Inv) <= Inverse_Limit
       and then
         (R (U) - R (Prepared.Terms (Index).Real_Part)) * (R (U) - R (Prepared.Terms (Index).Real_Part))
         + R (Prepared.Terms (Index).Imaginary_Part) * R (Prepared.Terms (Index).Imaginary_Part)
         >= Smallest_Square
       and then
         abs (R (Sq)
              - ((R (U) - R (Prepared.Terms (Index).Real_Part)) * (R (U) - R (Prepared.Terms (Index).Real_Part))
                 + R (Prepared.Terms (Index).Imaginary_Part) * R (Prepared.Terms (Index).Imaginary_Part)))
         <= Square_Error
            * ((R (U) - R (Prepared.Terms (Index).Real_Part)) * (R (U) - R (Prepared.Terms (Index).Real_Part))
               + R (Prepared.Terms (Index).Imaginary_Part) * R (Prepared.Terms (Index).Imaginary_Part))
       and then
         abs (R (Inv)
              - 1.0
                / ((R (U) - R (Prepared.Terms (Index).Real_Part)) * (R (U) - R (Prepared.Terms (Index).Real_Part))
                   + R (Prepared.Terms (Index).Imaginary_Part) * R (Prepared.Terms (Index).Imaginary_Part)))
         <= Inverse_Error
            * (1.0
               / ((R (U) - R (Prepared.Terms (Index).Real_Part)) * (R (U) - R (Prepared.Terms (Index).Real_Part))
                  + R (Prepared.Terms (Index).Imaginary_Part) * R (Prepared.Terms (Index).Imaginary_Part)))
       and then Q > 0.0
       and then Q <= Largest_Partial_Product
       and then Amp >= 16.0
       and then R (Amp) <= 16.0 + To_Real (Index - 1) * Amplification_Step
       and then (if Accurate then Is_Setup_Accurate (Warp, R (Node), U, Ratio))
       and then Is_Q_State (Warp, Cache, Denominator_Scale, R (Node), Accurate, Index - 1, Q, Amp),
     Post =>
       abs (Sq * Prepared.Terms (Index).Reciprocal_Norm) <= 2.0**203
       and then abs (Ratio * Ratio) <= 2.0**101
       and then abs ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm)) <= 2.0**304
       and then abs (Q * ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm))) <= 2.0**640
       and then Amp <= 2.0**206
       and then Dimensionless'Max (1.0, Inv) <= 2.0**201
       and then Amp + Dimensionless'Max (1.0, Inv) >= 16.0
       and then R (Amp + Dimensionless'Max (1.0, Inv)) <= 16.0 + To_Real (Index) * Amplification_Step
       and then
         (if Q * ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm)) >= Smallest_Partial_Product
          then
            Is_Q_State
              (Warp,
               Cache,
               Denominator_Scale,
               R (Node),
               Accurate,
               Index,
               Q * ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm)),
               Amp + Dimensionless'Max (1.0, Inv)));
   --  The factor of a pair: nothing overflows, and the running product stays accurate unless it falls below
   --  its lower limit.

   procedure Lemma_Pair_Range_Of_Term
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term; U, Sq : Dimensionless)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Pair <= Cache.Pair_Count
       and then Poles.Is_Term_Bounded (Term)
       and then Poles.Is_Pair_Term (Cache, Pair, Term)
       and then U >= 0.0
       and then U <= 1.0
       and then Sq >= 2.0**(-201)
       and then Sq <= 2.0**403
       and then
         abs (R (Sq)
              - ((R (U) - R (Term.Real_Part)) * (R (U) - R (Term.Real_Part))
                 + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
         <= Square_Error
            * ((R (U) - R (Term.Real_Part)) * (R (U) - R (Term.Real_Part))
               + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)),
     Post => abs Term.Reciprocal_Norm <= Magnitude_Limit and then abs (Sq * Term.Reciprocal_Norm) <= 2.0**203;
   --  The product of the computed squared distance from a pair and the computed reciprocal of |π|² is moderate.

   procedure Lemma_Pair_Range_Of_Term
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term; U, Sq : Dimensionless)
   is
      pragma Unreferenced (Cache, Pair);
      RN : constant Dimensionless := Term.Reciprocal_Norm;
      A  : constant Real := R (Term.Real_Part);
      B  : constant Real := R (Term.Imaginary_Part);
      Z  : constant Real := R (U) - A;
   begin
      pragma Assert (abs RN <= Magnitude_Limit);
      Lemma_Limit (RN);
      pragma Assert (B > 0.0);
      pragma Assert (B * B > 0.0);
      pragma Assert (A * A >= 0.0);
      pragma Assert (A * A + B * B > 0.0);
      pragma Assert (abs (R (RN) - 1.0 / (A * A + B * B)) <= 8.0 * Epsilon * (1.0 / (A * A + B * B)));
      pragma Assert (R (U) >= 0.0 and then R (U) <= 1.0);
      pragma Assert (abs Z <= 1.0 + abs A);
      Lemma_Pair_Factor_Range (Z, A, B, R (Sq), R (RN));
      Lemma_Pair_Product_Limit (Sq, RN);
   end Lemma_Pair_Range_Of_Term;

   procedure Lemma_Pair_Factor
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Node              : Dimensionless;
      Accurate          : Boolean;
      Index             : Rational_Degree_Slot;
      U, Ratio, Sq, Inv : Dimensionless;
      Q, Amp            : Dimensionless)
   is
      RN : constant Dimensionless := Prepared.Terms (Index).Reciprocal_Norm;
   begin
      Lemma_Term_Kind (Cache, Prepared, Index);
      Lemma_Pair_Range_Of_Term (Cache, Index - Cache.Real_Pole_Count, Prepared.Terms (Index), U, Sq);
      Lemma_Pair_Factor_Ranges (Ratio, Sq, RN, Q);
      Lemma_Maximum (Inv);
      pragma Assert (R (Dimensionless'Max (1.0, Inv)) <= Inverse_Limit);
      Lemma_Amplification (Amp, Dimensionless'Max (1.0, Inv), Index);
      if Accurate and then Amp + Dimensionless'Max (1.0, Inv) <= Largest_Amplification then
         if Q * ((Ratio * Ratio) * (Sq * RN)) >= Smallest_Partial_Product then
            Lemma_Pair_Factor_Accuracy
              (Warp, Cache, Denominator_Scale, Prepared, Node, Index, U, Ratio, Sq, Inv, Q, Amp);
         end if;
      else
         Lemma_Q_State_Intro
           (Warp,
            Cache,
            Denominator_Scale,
            R (Node),
            Accurate,
            Index,
            Q * ((Ratio * Ratio) * (Sq * RN)),
            Amp + Dimensionless'Max (1.0, Inv));
      end if;
   end Lemma_Pair_Factor;

   ---------------------------------------------------------------------------
   --  Before and after the node loop
   ---------------------------------------------------------------------------

   type Axis_Reals is array (Axis_Name) of Real with Ghost;
   --  The exact sum of the magnitudes of the computed terms, on every axis.

   procedure Lemma_Setup (Warp : Dimensionless; Sample : Sample_Nodes.Sample)
   with
     Ghost,
     Pre  => Warp >= Smallest_Warp and then Warp <= Largest_Warp and then Sample_Nodes.Is_In_Range (Sample),
     Post =>
       (if Sample_Nodes.Is_Accurate (Sample)
        then
          Is_Setup_Accurate
            (Warp,
             R (Sample.Node),
             Dimensionless'Min (1.0, Sample.Node / (Warp * Sample.Complement + Sample.Node)),
             (Warp * Sample.Complement + Sample.Node) / Warp));
   --  At an accurate sample the computed distance and ratio are accurate.

   procedure Lemma_Setup (Warp : Dimensionless; Sample : Sample_Nodes.Sample) is
   begin
      Lemma_Setup_Ranges (Warp, Sample.Node, Sample.Complement);
      if Sample_Nodes.Is_Accurate (Sample) then
         Sample_Nodes.Lemma_Complement_Is_Relative (Sample);
         Lemma_Setup_Accuracy (Warp, Sample.Node, Sample.Complement);
      end if;
   end Lemma_Setup;

   procedure Lemma_Start
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      V                 : Real;
      Accurate          : Boolean;
      U                 : Dimensionless)
   with
     Ghost,
     Pre  => Has_Layout (Cache),
     Post =>
       (for all Axis in Axis_Name => Is_Axis_State (Prepared, 0, Axis, U, 0.0, 0.0, 0.0))
       and then Is_Q_State (Warp, Cache, Denominator_Scale, V, Accurate, 0, Denominator_Scale, 16.0);
   --  The state of the node loop before its first pole.

   procedure Lemma_Start
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      V                 : Real;
      Accurate          : Boolean;
      U                 : Dimensionless)
   is
      pragma Unreferenced (Accurate);
   begin
      Lemma_Partial_Start (Cache, Denominator_Scale, Warp, V);
      pragma Assert (R (16.0) = 16.0);
      pragma Assert (R (0.0) = 0.0);
      pragma Assert (To_Real (0) = 0.0);
      for Axis in Axis_Name loop
         pragma Assert (Term_Sum (Prepared, 0, Axis, R (U)) = 0.0);
         pragma Assert (Poles.Slope_Sum (Prepared, 0, Axis) = 0.0);
         pragma Assert (Is_Axis_State (Prepared, 0, Axis, U, 0.0, 0.0, 0.0));
         pragma Loop_Invariant (for all A in X_Axis .. Axis => Is_Axis_State (Prepared, 0, A, U, 0.0, 0.0, 0.0));
      end loop;
   end Lemma_Start;

   procedure Lemma_Axis_State_Elim
     (Prepared : Preparation; Count : Natural; Axis : Axis_Name; U, Ret, Size : Dimensionless; M : Real)
   with
     Ghost,
     Pre  =>
       Count <= Maximum_Rational_Degree
       and then Prepared.Active (Axis)
       and then Is_Axis_State (Prepared, Count, Axis, U, Ret, Size, M),
     Post =>
       Size >= 0.0
       and then R (Size) >= 0.0
       and then M >= 0.0
       and then
         abs (R (Ret) - Term_Sum (Prepared, Count, Axis, R (U)))
         <= Alpha (Count) * M + Zeta * Poles.Slope_Sum (Prepared, Count, Axis) + Dust (Count)
       and then M <= Gamma (Count) * R (Size) + Dust (Count);
   --  What Is_Axis_State says of an active axis.

   procedure Lemma_Axis_State_Elim
     (Prepared : Preparation; Count : Natural; Axis : Axis_Name; U, Ret, Size : Dimensionless; M : Real)
   is
      pragma Unreferenced (Prepared, Count, Axis, U, Ret, M);
   begin
      pragma Assert (Size >= 0.0);
      Lemma_Nonnegative (Size);
   end Lemma_Axis_State_Elim;

   procedure Lemma_Final_Tangent (Error, Shift, Gap, M, Size, SS, Slope, Al, Ga, Du, WU, WE : Real)
   with
     Ghost,
     Pre  =>
       Error <= Al * M + Zeta * SS + Du
       and then Shift <= Distance_Error * SS
       and then Gap <= Unit * WE + 16.0 * Gap_Dust
       and then WE <= WU + Distance_Error * SS
       and then WU <= Weight_Gain * M + Zeta * SS
       and then M >= 0.0
       and then Size >= 0.0
       and then M <= Ga * Size + Du
       and then SS >= 0.0
       and then SS <= Slope
       and then Al >= 0.0
       and then Al <= 28.1 * Epsilon
       and then Ga >= 1.0
       and then Ga <= 1.0 + 36.0 * Epsilon
       and then Du >= 0.0
       and then Du <= 128.0 * Eta,
     Post => Error + Shift + Gap <= Term_Slack * Size + Slope_Slack * Slope + Retained_Dust;
   --  The allowances cover the accumulated rounding of the terms, the move to the exact distance and the gap
   --  between the gathered terms and those of the cache, whose weights are WU at the computed distance and WE at
   --  the exact one.

   procedure Lemma_Final_Tangent (Error, Shift, Gap, M, Size, SS, Slope, Al, Ga, Du, WU, WE : Real) is
      pragma Unreferenced (WU);
   begin
      Lemma_Scale (Al, 28.1 * Epsilon, M);
      Lemma_Scale (Ga, 1.0 + 36.0 * Epsilon, Size);
      pragma Assert (M <= (1.0 + 36.0 * Epsilon) * Size + Du);
      pragma Assert (WE <= Weight_Gain * M + (Zeta + Distance_Error) * SS);
      pragma Assert (Unit * WE <= Unit * (Weight_Gain * M) + Unit * ((Zeta + Distance_Error) * SS));
      pragma Assert (Unit * (Weight_Gain * M) <= 1.002 * Epsilon * M);
      pragma Assert (Unit * ((Zeta + Distance_Error) * SS) <= 0.01 * Epsilon * SS);
      pragma Assert (Gap <= 1.002 * Epsilon * M + 0.01 * Epsilon * SS + 16.0 * Gap_Dust);
      pragma Assert (Al * M + 1.002 * Epsilon * M <= 29.11 * Epsilon * ((1.0 + 36.0 * Epsilon) * Size + Du));
      pragma Assert (Al * M + 1.002 * Epsilon * M <= 29.2 * Epsilon * Size + Du);
      pragma Assert (Zeta * SS + Distance_Error * SS + 0.01 * Epsilon * SS <= 5.02 * Epsilon * SS);
      pragma
        Assert (Error + Shift + Gap <= 29.2 * Epsilon * Size + 5.02 * Epsilon * Slope + 2.0 * Du + 16.0 * Gap_Dust);
      pragma Assert (2.0 * Du + 16.0 * Gap_Dust <= Retained_Dust);
   end Lemma_Final_Tangent;

   procedure Lemma_Final_Denominator (Q, Exact, Amp : Real)
   with
     Ghost,
     Pre  =>
       Q > 0.0
       and then Amp >= 16.0
       and then Amp <= Amplification_Bound
       and then abs (Q - Exact) <= Lambda * (Amp - 16.0) * abs Exact,
     Post => abs (Q - Exact) <= Factor_Slack * Amp * Q;
   --  The error of the computed Q, relative to the computed Q instead of the exact one.

   procedure Lemma_Final_Denominator (Q, Exact, Amp : Real) is
      Rho : constant Real := Lambda * (Amp - 16.0);
   begin
      pragma Assert (Rho >= 0.0 and then Rho <= 0.001);
      Lemma_Scale (1.0, 1.002 * (1.0 - Rho), Rho);
      pragma Assert (abs (Exact - Q) <= Rho * abs Exact);
      pragma Assert ((1.002 * (1.0 - Rho)) * Rho = (1.002 * Rho) * (1.0 - Rho));
      Lemma_Relative_Swap (Exact, Q, Rho, 1.002 * Rho);
      pragma Assert (abs Q = Q);
      pragma Assert (1.002 * Rho <= Factor_Slack * Amp);
      Lemma_Scale (1.002 * Rho, Factor_Slack * Amp, Q);
   end Lemma_Final_Denominator;

   procedure Lemma_Coefficient_Bounds (Count : Natural)
   with
     Ghost,
     Pre  => Count <= Maximum_Rational_Degree,
     Post =>
       To_Real (Count) >= 0.0
       and then To_Real (Count) <= 16.0
       and then Alpha (Count) >= 0.0
       and then Alpha (Count) <= 28.1 * Epsilon
       and then Gamma (Count) >= 1.0
       and then Gamma (Count) <= 1.0 + 36.0 * Epsilon
       and then Dust (Count) >= 0.0
       and then Dust (Count) <= 128.0 * Eta;

   procedure Lemma_Coefficient_Bounds (Count : Natural) is
   begin
      pragma Assert (To_Real (Count) >= 0.0);
      pragma Assert (To_Real (Count) <= 16.0);
      pragma Assert (Dust_Step = 8.0 * Eta);
   end Lemma_Coefficient_Bounds;

   procedure Lemma_Final_Axis
     (Warp      : Dimensionless;
      Cache     : Rational_Antiderivative;
      Prepared  : Preparation;
      Node      : Dimensionless;
      Axis      : Axis_Name;
      U, Ratio  : Dimensionless;
      Ret, Size : Dimensionless;
      M         : Real)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then Poles.Is_Cache_Prepared (Cache, Prepared)
       and then U >= 0.0
       and then U <= 1.0
       and then Prepared.Active (Axis)
       and then Is_Setup_Accurate (Warp, R (Node), U, Ratio)
       and then Is_Axis_State (Prepared, Prepared.Count, Axis, U, Ret, Size, M)
       and then Is_Weight_State (Prepared, Prepared.Count, Axis, U, M),
     Post =>
       abs (R (Ret)
            + R (Cache.Constant_Tangent (Axis))
            - Retained_Tangent.Value (Cache, Axis, Model.Distance (Warp, R (Node))))
       <= Term_Slack * R (Size) + Slope_Slack * R (Prepared.Slope (Axis)) + Retained_Dust;
   --  The accuracy of the retained tangent on one active axis, as Is_Node_Accurate states it.

   procedure Lemma_Final_Axis
     (Warp      : Dimensionless;
      Cache     : Rational_Antiderivative;
      Prepared  : Preparation;
      Node      : Dimensionless;
      Axis      : Axis_Name;
      U, Ratio  : Dimensionless;
      Ret, Size : Dimensionless;
      M         : Real)
   is
      Count         : constant Natural := Prepared.Count;
      Ue            : constant Real := Model.Distance (Warp, R (Node));
      At_U          : constant Real := Term_Sum (Prepared, Count, Axis, R (U));
      At_Ue         : constant Real := Term_Sum (Prepared, Count, Axis, Ue);
      WU            : constant Real := Weight_Sum (Prepared, Count, Axis, R (U));
      WE            : constant Real := Weight_Sum (Prepared, Count, Axis, Ue);
      SS            : constant Real := Poles.Slope_Sum (Prepared, Count, Axis);
      Constant_Part : constant Real := R (Cache.Constant_Tangent (Axis));
      Cached        : constant Real :=
        Real_Sum (Cache, Cache.Real_Pole_Count, Axis, Ue) + Pair_Sum (Cache, Cache.Pair_Count, Axis, Ue);
   begin
      Lemma_Coefficient_Bounds (Count);
      Lemma_Axis_State_Elim (Prepared, Count, Axis, U, Ret, Size, M);
      pragma Assert (WU <= Weight_Gain * M + Zeta * SS);
      Lemma_Setup_Elim (Warp, R (Node), U, Ratio);
      pragma Assert (R (U) >= 0.0 and then R (U) <= 1.0);
      Lemma_Term_Sum_Shift (Cache, Prepared, Count, Axis, R (U), Ue);
      Lemma_Scale (abs (R (U) - Ue), Distance_Error, SS);
      pragma Assert (abs (At_U - At_Ue) <= Distance_Error * SS);
      Lemma_Weight_Sum_Shift (Cache, Prepared, Count, Axis, Ue, R (U));
      pragma Assert (abs (Ue - R (U)) = abs (R (U) - Ue));
      pragma Assert (WE <= WU + Distance_Error * SS);
      Lemma_Term_Sum_Gap (Cache, Prepared, Count, Axis, Ue);
      pragma Assert (Real_Count (Cache, Count) = Cache.Real_Pole_Count);
      pragma Assert (Pair_Count (Cache, Count) = Cache.Pair_Count);
      pragma Assert (To_Real (Count) * Gap_Dust <= 16.0 * Gap_Dust);
      pragma Assert (abs (At_Ue - Cached) <= Unit * WE + 16.0 * Gap_Dust);
      pragma Assert (Retained_Tangent.Value (Cache, Axis, Ue) = Constant_Part + Cached);
      pragma Assert (SS <= R (Prepared.Slope (Axis)));
      pragma Assert (R (Size) >= 0.0);
      Lemma_Final_Tangent
        (abs (R (Ret) - At_U),
         abs (At_U - At_Ue),
         abs (At_Ue - Cached),
         M,
         R (Size),
         SS,
         R (Prepared.Slope (Axis)),
         Alpha (Count),
         Gamma (Count),
         Dust (Count),
         WU,
         WE);
      pragma
        Assert
          (abs (R (Ret) + Constant_Part - (Constant_Part + Cached))
           <= abs (R (Ret) - At_U) + abs (At_U - At_Ue) + abs (At_Ue - Cached));
   end Lemma_Final_Axis;

   procedure Lemma_Finish
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample;
      U, Ratio          : Dimensionless;
      Node              : Retained_Node;
      Magnitude         : Axis_Reals)
   with
     Ghost,
     Pre  =>
       Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then Poles.Is_Cache_Prepared (Cache, Prepared)
       and then U >= 0.0
       and then U <= 1.0
       and then (if Sample_Nodes.Is_Accurate (Sample) then Is_Setup_Accurate (Warp, R (Sample.Node), U, Ratio))
       and then Node.Cached_Q > 0.0
       and then Node.Amplification >= 16.0
       and then R (Node.Amplification) <= 16.0 + To_Real (Prepared.Count) * Amplification_Step
       and then
         (for all Axis in Axis_Name =>
            Is_Axis_State
              (Prepared, Prepared.Count, Axis, U, Node.Retained (Axis), Node.Retained_Size (Axis), Magnitude (Axis)))
       and then (for all Axis in Axis_Name => Is_Weight_State (Prepared, Prepared.Count, Axis, U, Magnitude (Axis)))
       and then
         Is_Q_State
           (Warp,
            Cache,
            Denominator_Scale,
            R (Sample.Node),
            Sample_Nodes.Is_Accurate (Sample),
            Prepared.Count,
            Node.Cached_Q,
            Node.Amplification),
     Post =>
       Node.Amplification <= Largest_Amplification_Value
       and then
         (for all Axis in Axis_Name =>
            abs Node.Retained (Axis) <= Largest_Retained_Value
            and then Node.Retained_Size (Axis) >= 0.0
            and then Node.Retained_Size (Axis) <= Largest_Retained_Value)
       and then
         (if Sample_Nodes.Is_Accurate (Sample) and then Node.Amplification <= Largest_Amplification
          then Is_Node_Accurate (Warp, Cache, Denominator_Scale, Prepared, Node, R (Sample.Node)));
   --  What the node loop has maintained gives the postcondition of At_Node.

   procedure Lemma_Finish
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample;
      U, Ratio          : Dimensionless;
      Node              : Retained_Node;
      Magnitude         : Axis_Reals)
   is
      Count : constant Natural := Prepared.Count;
      V     : constant Real := R (Sample.Node);
   begin
      Lemma_Coefficient_Bounds (Count);
      pragma Assert (R (Node.Amplification) <= 16.0 + 16.0 * Amplification_Step);
      pragma Assert (Node.Amplification <= Largest_Amplification_Value);
      for Axis in Axis_Name loop
         pragma
           Assert
             (Is_Axis_State
                (Prepared, Count, Axis, U, Node.Retained (Axis), Node.Retained_Size (Axis), Magnitude (Axis)));
         pragma Assert (abs (R (Node.Retained (Axis))) <= Sum_Limit);
         pragma Assert (R (Node.Retained_Size (Axis)) <= Sum_Limit);
         pragma Assert (abs Node.Retained (Axis) <= Largest_Retained_Value);
         pragma Assert (Node.Retained_Size (Axis) <= Largest_Retained_Value);
         pragma
           Loop_Invariant
             (for all A in X_Axis .. Axis =>
                abs Node.Retained (A) <= Largest_Retained_Value
                and then Node.Retained_Size (A) >= 0.0
                and then Node.Retained_Size (A) <= Largest_Retained_Value);
      end loop;
      if Sample_Nodes.Is_Accurate (Sample) and then Node.Amplification <= Largest_Amplification then
         Lemma_Partial_Start (Cache, Denominator_Scale, Warp, V);
         pragma Assert (Count = Cache.Real_Pole_Count + Cache.Pair_Count);
         Lemma_Final_Denominator
           (R (Node.Cached_Q), Model.Cached_Denominator (Cache, Denominator_Scale, Warp, V), R (Node.Amplification));
         for Axis in Axis_Name loop
            if Prepared.Active (Axis) then
               pragma Assert (Is_Weight_State (Prepared, Count, Axis, U, Magnitude (Axis)));
               Lemma_Final_Axis
                 (Warp,
                  Cache,
                  Prepared,
                  Sample.Node,
                  Axis,
                  U,
                  Ratio,
                  Node.Retained (Axis),
                  Node.Retained_Size (Axis),
                  Magnitude (Axis));
            end if;
            pragma
              Loop_Invariant
                (for all A in X_Axis .. Axis =>
                   (if Prepared.Active (A)
                    then
                      abs (R (Node.Retained (A))
                           + R (Cache.Constant_Tangent (A))
                           - Retained_Tangent.Value (Cache, A, Model.Distance (Warp, V)))
                      <= Term_Slack * R (Node.Retained_Size (A))
                         + Slope_Slack * R (Prepared.Slope (A))
                         + Retained_Dust));
         end loop;
      end if;
   end Lemma_Finish;

   ---------------------------------------------------------------------------
   --  What the node loop sees
   ---------------------------------------------------------------------------

   procedure Lemma_Within_Sum_Limit (Value : Dimensionless)
   with Ghost, Pre => abs (R (Value)) <= Sum_Limit, Post => abs Value <= 2.0**610;

   procedure Lemma_Within_Sum_Limit (Value : Dimensionless) is null;

   procedure Lemma_Axis_State_Bounds
     (Prepared : Preparation; Count : Natural; Axis : Axis_Name; U, Ret, Size : Dimensionless; M : Real)
   with
     Ghost,
     Pre  => Count <= Maximum_Rational_Degree and then Is_Axis_State (Prepared, Count, Axis, U, Ret, Size, M),
     Post => abs Ret <= 2.0**610 and then abs Size <= 2.0**610;
   --  No partial sum of the node loop is large.

   procedure Lemma_Axis_State_Bounds
     (Prepared : Preparation; Count : Natural; Axis : Axis_Name; U, Ret, Size : Dimensionless; M : Real)
   is
      pragma Unreferenced (Prepared, Axis, U, M);
   begin
      Lemma_Coefficient_Bounds (Count);
      pragma Assert (abs (R (Ret)) <= To_Real (Count) * Term_Limit);
      pragma Assert (abs (R (Ret)) <= Sum_Limit);
      Lemma_Within_Sum_Limit (Ret);
      Lemma_Nonnegative (Size);
      pragma Assert (R (Size) <= To_Real (Count) * Term_Limit);
      pragma Assert (abs (R (Size)) <= Sum_Limit);
      Lemma_Within_Sum_Limit (Size);
   end Lemma_Axis_State_Bounds;

   procedure Lemma_Signs (Q, Amp : Dimensionless)
   with Ghost, Pre => Q > 0.0 and then Amp >= 16.0, Post => R (Q) > 0.0 and then R (Amp) >= 16.0;

   procedure Lemma_Signs (Q, Amp : Dimensionless) is null;

   package Steps
     with Ghost
   is
      --  The predicates which At_Node hands from one lemma to the next, and the lemmas which it calls.
      --
      --  At_Node performs some twenty-five floating-point operations for a pole, and every one of them is in
      --  the context of whatever is to be proved after it. The provers take seconds to do anything with so many
      --  operations, a few divisions among them, unless what is to be proved is already there, word for word. So
      --  the predicates are completed in the body of this package, where At_Node sees none of their definitions,
      --  and every lemma states that the result of each operation which follows it is in its type, in exactly
      --  the words of the overflow check. The proof obligations of At_Node are then a matter of matching the
      --  postcondition of one lemma with the precondition of the next and with those checks.

      function Is_Set_Up (Warp : Dimensionless; Sample : Sample_Nodes.Sample; U, Ratio : Dimensionless) return Boolean;
      --  U and Ratio are the computed distance and the computed h/W of the sample: in range, and accurate if
      --  the sample is.

      function Is_Axis_Sound
        (Prepared : Preparation; Count : Natural; Axis : Axis_Name; U, Ret, Size : Dimensionless; M : Real)
         return Boolean;
      --  Is_Axis_State and Is_Weight_State.

      function Is_Product_Sound
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Sample            : Sample_Nodes.Sample;
         Count             : Natural;
         Q, Amp            : Dimensionless) return Boolean
      with Pre => Has_Layout (Cache);
      --  Is_Q_State; the running product is positive and within its upper limit, and the amplification is at
      --  least sixteen and at most what Count poles can make it. Before the first pole the running product is the
      --  denominator scale, and within its lower limit if that is.

      function Is_Pole_Found (U : Dimensionless; Term : Pole_Term; X : Dimensionless) return Boolean;
      --  X is the rounded difference of the computed distance U and the real part of the gathered pole Term.

      function Is_Real_Pole_Sound (U : Dimensionless; Term : Pole_Term; X, Rec : Dimensionless) return Boolean;
      --  Term is a real pole, X the computed U - p and Rec its computed reciprocal: bounded and accurate.

      function Is_Pair_Sound (U : Dimensionless; Term : Pole_Term; X, Sq, Inv, Wr, Wi : Dimensionless) return Boolean;
      --  Term is a pair, X the computed U - A, Sq the computed squared distance, Inv its computed reciprocal and
      --  Wr, Wi the two computed weights: bounded and accurate.

      function Is_Finished
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Prepared          : Preparation;
         Sample            : Sample_Nodes.Sample;
         Retained          : Axis_Values;
         Retained_Size     : Axis_Values;
         Cached_Q          : Dimensionless;
         Amplification     : Dimensionless) return Boolean
      with Pre => Has_Layout (Cache);
      --  The node which At_Node returns with these values satisfies its postcondition.

      function Real_Magnitude (M : Real; Residue, Rec : Dimensionless) return Real;
      --  M and the magnitude of the term of a real pole: the residue times the computed reciprocal.

      function Pair_Magnitude (M : Real; Rr, Ri, Wr, Wi : Dimensionless) return Real;
      --  M and the magnitude of the term of a pair: its two residues times the two computed weights.

      function Setup_Is_Finite (Warp : Dimensionless; Sample : Sample_Nodes.Sample) return Boolean
      with
        Pre  => Warp >= Smallest_Warp and then Warp <= Largest_Warp and then Sample_Nodes.Is_In_Range (Sample),
        Post =>
          Setup_Is_Finite'Result
          and then
            (if Setup_Is_Finite'Result
             then
               (Warp * Sample.Complement) in Dimensionless
               and then (Warp * Sample.Complement + Sample.Node) in Dimensionless
               and then Warp * Sample.Complement + Sample.Node /= 0.0
               and then (Sample.Node / (Warp * Sample.Complement + Sample.Node)) in Dimensionless
               and then Warp /= 0.0
               and then ((Warp * Sample.Complement + Sample.Node) / Warp) in Dimensionless);
      --  Always true. At_Node declares a constant with this value before anything else, which is the one way to
      --  have a lemma in force for its other declarations: nothing overflows in the operations before the loop.

      procedure Lemma_Distance (Warp : Dimensionless; Sample : Sample_Nodes.Sample)
      with
        Pre  => Warp >= Smallest_Warp and then Warp <= Largest_Warp and then Sample_Nodes.Is_In_Range (Sample),
        Post =>
          Is_Set_Up
            (Warp,
             Sample,
             Dimensionless'Min (1.0, Sample.Node / (Warp * Sample.Complement + Sample.Node)),
             (Warp * Sample.Complement + Sample.Node) / Warp);
      --  What At_Node computes before its loop.

      procedure Lemma_First
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Prepared          : Preparation;
         Sample            : Sample_Nodes.Sample;
         U                 : Dimensionless)
      with
        Pre  => Has_Layout (Cache) and then Denominator_Scale > 0.0 and then Denominator_Scale <= Magnitude_Limit,
        Post =>
          Is_Axis_Sound (Prepared, 0, X_Axis, U, 0.0, 0.0, 0.0)
          and then Is_Axis_Sound (Prepared, 0, Y_Axis, U, 0.0, 0.0, 0.0)
          and then Is_Axis_Sound (Prepared, 0, Z_Axis, U, 0.0, 0.0, 0.0)
          and then Is_Axis_Sound (Prepared, 0, E_Axis, U, 0.0, 0.0, 0.0)
          and then Is_Product_Sound (Warp, Cache, Denominator_Scale, Sample, 0, Denominator_Scale, 16.0);
      --  The state of the loop before its first pole.

      procedure Lemma_Pole
        (Warp     : Dimensionless;
         Sample   : Sample_Nodes.Sample;
         Cache    : Rational_Antiderivative;
         Prepared : Preparation;
         Index    : Rational_Degree_Slot;
         U, Ratio : Dimensionless)
      with
        Pre  =>
          Has_Layout (Cache)
          and then Poles.Is_Cache_Prepared (Cache, Prepared)
          and then Index <= Prepared.Count
          and then Is_Set_Up (Warp, Sample, U, Ratio),
        Post =>
          (U - Prepared.Terms (Index).Real_Part) in Dimensionless
          and then Is_Pole_Found (U, Prepared.Terms (Index), U - Prepared.Terms (Index).Real_Part);
      --  The difference of the computed distance and the real part of a pole.

      procedure Lemma_Real_Reach (U : Dimensionless; Term : Pole_Term; X : Dimensionless)
      with
        Pre  => Is_Pole_Found (U, Term, X) and then Term.Imaginary_Part = 0.0,
        Post => X /= 0.0 and then (1.0 / X) in Dimensionless and then Is_Real_Pole_Sound (U, Term, X, 1.0 / X);
      --  The reciprocal of the distance from a real pole.

      procedure Lemma_Pair_Reach (U : Dimensionless; Term : Pole_Term; X : Dimensionless)
      with
        Pre  => Is_Pole_Found (U, Term, X) and then Term.Imaginary_Part /= 0.0,
        Post =>
          (X * X) in Dimensionless
          and then (Term.Imaginary_Part * Term.Imaginary_Part) in Dimensionless
          and then (X * X + Term.Imaginary_Part * Term.Imaginary_Part) in Dimensionless
          and then X * X + Term.Imaginary_Part * Term.Imaginary_Part /= 0.0
          and then (1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part)) in Dimensionless
          and then (X * (1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part))) in Dimensionless
          and then (Term.Imaginary_Part * (1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part))) in Dimensionless
          and then
            Is_Pair_Sound
              (U,
               Term,
               X,
               X * X + Term.Imaginary_Part * Term.Imaginary_Part,
               1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part),
               X * (1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part)),
               Term.Imaginary_Part * (1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part)));
      --  The squared distance from a pair, its reciprocal and the two weights.

      procedure Lemma_Idle
        (Prepared  : Preparation;
         Index     : Rational_Degree_Slot;
         Axis      : Axis_Name;
         U         : Dimensionless;
         Ret, Size : Dimensionless;
         M         : Real)
      with
        Pre  => Is_Axis_Sound (Prepared, Index - 1, Axis, U, Ret, Size, M),
        Post => (if not Prepared.Active (Axis) then Is_Axis_Sound (Prepared, Index, Axis, U, Ret, Size, M));
      --  An axis which is not active keeps its state.

      procedure Lemma_Real_Sum
        (Prepared  : Preparation;
         Index     : Rational_Degree_Slot;
         Axis      : Axis_Name;
         U, X, Rec : Dimensionless;
         Ret, Size : Dimensionless;
         M         : Real)
      with
        Pre  =>
          Is_Real_Pole_Sound (U, Prepared.Terms (Index), X, Rec)
          and then Is_Axis_Sound (Prepared, Index - 1, Axis, U, Ret, Size, M),
        Post =>
          (Prepared.Terms (Index).Residue_Real (Axis) * Rec) in Dimensionless
          and then (Ret + Prepared.Terms (Index).Residue_Real (Axis) * Rec) in Dimensionless
          and then (abs Prepared.Terms (Index).Residue_Real (Axis) * abs Rec) in Dimensionless
          and then (Size + abs Prepared.Terms (Index).Residue_Real (Axis) * abs Rec) in Dimensionless
          and then
            Is_Axis_Sound
              (Prepared,
               Index,
               Axis,
               U,
               Ret + Prepared.Terms (Index).Residue_Real (Axis) * Rec,
               Size + abs Prepared.Terms (Index).Residue_Real (Axis) * abs Rec,
               Real_Magnitude (M, Prepared.Terms (Index).Residue_Real (Axis), Rec));
      --  The term of a real pole on one axis.

      procedure Lemma_Pair_Sum
        (Prepared              : Preparation;
         Index                 : Rational_Degree_Slot;
         Axis                  : Axis_Name;
         U, X, Sq, Inv, Wr, Wi : Dimensionless;
         Ret, Size             : Dimensionless;
         M                     : Real)
      with
        Pre  =>
          Is_Pair_Sound (U, Prepared.Terms (Index), X, Sq, Inv, Wr, Wi)
          and then Is_Axis_Sound (Prepared, Index - 1, Axis, U, Ret, Size, M),
        Post =>
          (Prepared.Terms (Index).Residue_Real (Axis) * Wr) in Dimensionless
          and then (Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi) in Dimensionless
          and then
            (Prepared.Terms (Index).Residue_Real (Axis) * Wr - Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi)
            in Dimensionless
          and then
            (Ret
             + (Prepared.Terms (Index).Residue_Real (Axis) * Wr
                - Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi))
            in Dimensionless
          and then (abs Prepared.Terms (Index).Residue_Real (Axis) * abs Wr) in Dimensionless
          and then (abs Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi) in Dimensionless
          and then
            (abs Prepared.Terms (Index).Residue_Real (Axis) * abs Wr
             + abs Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi)
            in Dimensionless
          and then
            (Size
             + (abs Prepared.Terms (Index).Residue_Real (Axis) * abs Wr
                + abs Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi))
            in Dimensionless
          and then
            Is_Axis_Sound
              (Prepared,
               Index,
               Axis,
               U,
               Ret
               + (Prepared.Terms (Index).Residue_Real (Axis) * Wr
                  - Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi),
               Size
               + (abs Prepared.Terms (Index).Residue_Real (Axis) * abs Wr
                  + abs Prepared.Terms (Index).Residue_Imaginary (Axis) * Wi),
               Pair_Magnitude
                 (M,
                  Prepared.Terms (Index).Residue_Real (Axis),
                  Prepared.Terms (Index).Residue_Imaginary (Axis),
                  Wr,
                  Wi));
      --  The term of a pair on one axis.

      procedure Lemma_Real_Multiply
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Prepared          : Preparation;
         Sample            : Sample_Nodes.Sample;
         Index             : Rational_Degree_Slot;
         U, Ratio, X, Rec  : Dimensionless;
         Q, Amp            : Dimensionless)
      with
        Pre  =>
          Has_Layout (Cache)
          and then Poles.Is_Cache_Prepared (Cache, Prepared)
          and then Index <= Prepared.Count
          and then Is_Set_Up (Warp, Sample, U, Ratio)
          and then Is_Real_Pole_Sound (U, Prepared.Terms (Index), X, Rec)
          and then Is_Product_Sound (Warp, Cache, Denominator_Scale, Sample, Index - 1, Q, Amp),
        Post =>
          (X * Prepared.Terms (Index).Reciprocal_Norm) in Dimensionless
          and then (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm)) in Dimensionless
          and then (Q * (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm))) in Dimensionless
          and then (Amp + Dimensionless'Max (1.0, abs Rec)) in Dimensionless
          and then
            (if Q * (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm)) >= Smallest_Partial_Product
               and then Q * (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm)) <= Largest_Partial_Product
             then
               Is_Product_Sound
                 (Warp,
                  Cache,
                  Denominator_Scale,
                  Sample,
                  Index,
                  Q * (Ratio * (-X * Prepared.Terms (Index).Reciprocal_Norm)),
                  Amp + Dimensionless'Max (1.0, abs Rec)));
      --  The factor of a real pole.

      procedure Lemma_Pair_Multiply
        (Warp               : Dimensionless;
         Cache              : Rational_Antiderivative;
         Denominator_Scale  : Dimensionless;
         Prepared           : Preparation;
         Sample             : Sample_Nodes.Sample;
         Index              : Rational_Degree_Slot;
         U, Ratio           : Dimensionless;
         X, Sq, Inv, Wr, Wi : Dimensionless;
         Q, Amp             : Dimensionless)
      with
        Pre  =>
          Has_Layout (Cache)
          and then Poles.Is_Cache_Prepared (Cache, Prepared)
          and then Index <= Prepared.Count
          and then Is_Set_Up (Warp, Sample, U, Ratio)
          and then Is_Pair_Sound (U, Prepared.Terms (Index), X, Sq, Inv, Wr, Wi)
          and then Is_Product_Sound (Warp, Cache, Denominator_Scale, Sample, Index - 1, Q, Amp),
        Post =>
          (Ratio * Ratio) in Dimensionless
          and then (Sq * Prepared.Terms (Index).Reciprocal_Norm) in Dimensionless
          and then ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm)) in Dimensionless
          and then (Q * ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm))) in Dimensionless
          and then (Amp + Dimensionless'Max (1.0, Inv)) in Dimensionless
          and then
            (if Q * ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm)) >= Smallest_Partial_Product
               and then
                 Q * ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm)) <= Largest_Partial_Product
             then
               Is_Product_Sound
                 (Warp,
                  Cache,
                  Denominator_Scale,
                  Sample,
                  Index,
                  Q * ((Ratio * Ratio) * (Sq * Prepared.Terms (Index).Reciprocal_Norm)),
                  Amp + Dimensionless'Max (1.0, Inv)));
      --  The factor of a pair.

      procedure Lemma_Last
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Prepared          : Preparation;
         Sample            : Sample_Nodes.Sample;
         U, Ratio          : Dimensionless;
         Retained          : Axis_Values;
         Retained_Size     : Axis_Values;
         Cached_Q          : Dimensionless;
         Amplification     : Dimensionless;
         Magnitude         : Axis_Reals)
      with
        Pre  =>
          Has_Layout (Cache)
          and then Poles.Is_Cache_Prepared (Cache, Prepared)
          and then Is_Set_Up (Warp, Sample, U, Ratio)
          and then
            Is_Axis_Sound
              (Prepared, Prepared.Count, X_Axis, U, Retained (X_Axis), Retained_Size (X_Axis), Magnitude (X_Axis))
          and then
            Is_Axis_Sound
              (Prepared, Prepared.Count, Y_Axis, U, Retained (Y_Axis), Retained_Size (Y_Axis), Magnitude (Y_Axis))
          and then
            Is_Axis_Sound
              (Prepared, Prepared.Count, Z_Axis, U, Retained (Z_Axis), Retained_Size (Z_Axis), Magnitude (Z_Axis))
          and then
            Is_Axis_Sound
              (Prepared, Prepared.Count, E_Axis, U, Retained (E_Axis), Retained_Size (E_Axis), Magnitude (E_Axis))
          and then Is_Product_Sound (Warp, Cache, Denominator_Scale, Sample, Prepared.Count, Cached_Q, Amplification),
        Post =>
          Is_Finished
            (Warp, Cache, Denominator_Scale, Prepared, Sample, Retained, Retained_Size, Cached_Q, Amplification);
      --  What the loop has maintained gives the postcondition of At_Node.

      procedure Lemma_Result
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Prepared          : Preparation;
         Sample            : Sample_Nodes.Sample;
         Retained          : Axis_Values;
         Retained_Size     : Axis_Values;
         Cached_Q          : Dimensionless;
         Amplification     : Dimensionless)
      with
        Pre  =>
          Has_Layout (Cache)
          and then
            Is_Finished
              (Warp, Cache, Denominator_Scale, Prepared, Sample, Retained, Retained_Size, Cached_Q, Amplification)
          and then (Prepared.Count = 0 or else Cached_Q >= Smallest_Partial_Product),
        Post =>
          R (Cached_Q) > 0.0
          and then R (Amplification) >= 16.0
          and then Cached_Q > 0.0
          and then
            (if Prepared.Count > 0 or else Denominator_Scale >= Smallest_Partial_Product
             then Cached_Q >= Smallest_Partial_Product)
          and then Cached_Q <= Largest_Partial_Product
          and then Amplification >= 16.0
          and then Amplification <= Largest_Amplification_Value
          and then
            (for all Axis in Axis_Name =>
               abs Retained (Axis) <= Largest_Retained_Value
               and then Retained_Size (Axis) >= 0.0
               and then Retained_Size (Axis) <= Largest_Retained_Value)
          and then
            (if Sample_Nodes.Is_Accurate (Sample) and then Amplification <= Largest_Amplification
             then
               abs (R (Cached_Q) - Model.Cached_Denominator (Cache, Denominator_Scale, Warp, R (Sample.Node)))
               <= Factor_Slack * R (Amplification) * R (Cached_Q)
               and then
                 (for all Axis in Axis_Name =>
                    (if Prepared.Active (Axis)
                     then
                       abs (R (Retained (Axis))
                            + R (Cache.Constant_Tangent (Axis))
                            - Retained_Tangent.Value (Cache, Axis, Model.Distance (Warp, R (Sample.Node))))
                       <= Term_Slack * R (Retained_Size (Axis))
                          + Slope_Slack * R (Prepared.Slope (Axis))
                          + Retained_Dust)));
      --  What Is_Finished means: the postcondition of At_Node, in terms of the values which make the node it
      --  returns; the last part is Is_Node_Accurate, written out. The loop forgets everything but Is_Finished
      --  before this is called. The first two facts are the signs of the two factors of the allowance of Q, with
      --  which a prover needs one step to see that the product of the factors of the node is the product here.

   end Steps;

   package body Steps is

      function Is_Set_Up (Warp : Dimensionless; Sample : Sample_Nodes.Sample; U, Ratio : Dimensionless) return Boolean
      is (Warp >= Smallest_Warp
          and then Warp <= Largest_Warp
          and then U >= 0.0
          and then U <= 1.0
          and then Ratio >= 2.0**(-53)
          and then Ratio <= 2.0**50
          and then (if Sample_Nodes.Is_Accurate (Sample) then Is_Setup_Accurate (Warp, R (Sample.Node), U, Ratio)));

      function Is_Axis_Sound
        (Prepared : Preparation; Count : Natural; Axis : Axis_Name; U, Ret, Size : Dimensionless; M : Real)
         return Boolean
      is (Count <= Maximum_Rational_Degree
          and then Is_Axis_State (Prepared, Count, Axis, U, Ret, Size, M)
          and then Is_Weight_State (Prepared, Count, Axis, U, M));

      function Is_Product_Sound
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Sample            : Sample_Nodes.Sample;
         Count             : Natural;
         Q, Amp            : Dimensionless) return Boolean
      is (Count <= Cache.Real_Pole_Count + Cache.Pair_Count
          and then Q > 0.0
          and then Q <= Largest_Partial_Product
          and then Amp >= 16.0
          and then R (Amp) <= 16.0 + To_Real (Count) * Amplification_Step
          and then
            (if Count = 0 and then Denominator_Scale >= Smallest_Partial_Product then Q >= Smallest_Partial_Product)
          and then
            Is_Q_State
              (Warp, Cache, Denominator_Scale, R (Sample.Node), Sample_Nodes.Is_Accurate (Sample), Count, Q, Amp));

      function Is_Pole_Found (U : Dimensionless; Term : Pole_Term; X : Dimensionless) return Boolean
      is (U >= 0.0
          and then U <= 1.0
          and then Is_Term_Ready (Term)
          and then abs (R (X) - (R (U) - R (Term.Real_Part))) <= Epsilon * abs (R (U) - R (Term.Real_Part)) + Eta);

      function Is_Real_Pole_Sound (U : Dimensionless; Term : Pole_Term; X, Rec : Dimensionless) return Boolean
      is (U >= 0.0
          and then U <= 1.0
          and then Is_Term_Ready (Term)
          and then Term.Imaginary_Part = 0.0
          and then abs X >= 2.0**(-101)
          and then abs X <= 2.0**201
          and then abs Rec <= 2.0**101
          and then abs (R (U) - R (Term.Real_Part)) >= Reach
          and then abs (R (X) - (R (U) - R (Term.Real_Part))) <= Unit * abs (R (U) - R (Term.Real_Part))
          and then
            abs (R (Rec) - 1.0 / (R (U) - R (Term.Real_Part)))
            <= Reciprocal_Error * abs (1.0 / (R (U) - R (Term.Real_Part))));

      function Is_Pair_Sound (U : Dimensionless; Term : Pole_Term; X, Sq, Inv, Wr, Wi : Dimensionless) return Boolean
      is (U >= 0.0
          and then U <= 1.0
          and then Is_Term_Ready (Term)
          and then Term.Imaginary_Part > 0.0
          and then abs X <= 2.0**201
          and then Sq >= 2.0**(-201)
          and then Sq <= 2.0**403
          and then Inv >= 0.0
          and then Inv <= 2.0**201
          and then R (Inv) <= Inverse_Limit
          and then abs Wr <= 2.0**402
          and then Wi >= 0.0
          and then Wi <= 2.0**402
          and then
            (R (U) - R (Term.Real_Part)) * (R (U) - R (Term.Real_Part))
            + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)
            >= Smallest_Square
          and then
            abs (R (Sq)
                 - ((R (U) - R (Term.Real_Part)) * (R (U) - R (Term.Real_Part))
                    + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
            <= Square_Error
               * ((R (U) - R (Term.Real_Part)) * (R (U) - R (Term.Real_Part))
                  + R (Term.Imaginary_Part) * R (Term.Imaginary_Part))
          and then
            abs (R (Inv)
                 - 1.0
                   / ((R (U) - R (Term.Real_Part)) * (R (U) - R (Term.Real_Part))
                      + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
            <= Inverse_Error
               * (1.0
                  / ((R (U) - R (Term.Real_Part)) * (R (U) - R (Term.Real_Part))
                     + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
          and then Is_Pair_Weight (R (U) - R (Term.Real_Part), R (Term.Imaginary_Part), R (Inv), R (Wr), R (Wi)));

      function Is_Finished
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Prepared          : Preparation;
         Sample            : Sample_Nodes.Sample;
         Retained          : Axis_Values;
         Retained_Size     : Axis_Values;
         Cached_Q          : Dimensionless;
         Amplification     : Dimensionless) return Boolean
      is (R (Cached_Q) > 0.0
          and then R (Amplification) >= 16.0
          and then Cached_Q > 0.0
          and then
            (if Prepared.Count = 0 and then Denominator_Scale >= Smallest_Partial_Product
             then Cached_Q >= Smallest_Partial_Product)
          and then Cached_Q <= Largest_Partial_Product
          and then Amplification >= 16.0
          and then Amplification <= Largest_Amplification_Value
          and then
            (for all Axis in Axis_Name =>
               abs Retained (Axis) <= Largest_Retained_Value
               and then Retained_Size (Axis) >= 0.0
               and then Retained_Size (Axis) <= Largest_Retained_Value)
          and then
            (if Sample_Nodes.Is_Accurate (Sample) and then Amplification <= Largest_Amplification
             then
               abs (R (Cached_Q) - Model.Cached_Denominator (Cache, Denominator_Scale, Warp, R (Sample.Node)))
               <= Factor_Slack * R (Amplification) * R (Cached_Q)
               and then
                 (for all Axis in Axis_Name =>
                    (if Prepared.Active (Axis)
                     then
                       abs (R (Retained (Axis))
                            + R (Cache.Constant_Tangent (Axis))
                            - Retained_Tangent.Value (Cache, Axis, Model.Distance (Warp, R (Sample.Node))))
                       <= Term_Slack * R (Retained_Size (Axis))
                          + Slope_Slack * R (Prepared.Slope (Axis))
                          + Retained_Dust))));

      function Real_Magnitude (M : Real; Residue, Rec : Dimensionless) return Real
      is (M + abs (R (Residue)) * abs (R (Rec)));

      function Pair_Magnitude (M : Real; Rr, Ri, Wr, Wi : Dimensionless) return Real
      is (M + (abs (R (Rr)) * abs (R (Wr)) + abs (R (Ri)) * R (Wi)));

      function Setup_Is_Finite (Warp : Dimensionless; Sample : Sample_Nodes.Sample) return Boolean is
      begin
         Lemma_Half_Sum (Sample.Node, Sample.Complement);
         Lemma_Warp_Product (Warp, Sample.Complement);
         Lemma_Scale_Sum (Warp * Sample.Complement, Sample.Node);
         Lemma_Distance_Range (Sample.Node, Warp * Sample.Complement + Sample.Node);
         Lemma_Ratio_Range (Warp * Sample.Complement + Sample.Node, Warp);
         return True;
      end Setup_Is_Finite;

      procedure Lemma_Distance (Warp : Dimensionless; Sample : Sample_Nodes.Sample) is
      begin
         Lemma_Setup_Ranges (Warp, Sample.Node, Sample.Complement);
         Lemma_Setup (Warp, Sample);
      end Lemma_Distance;

      procedure Lemma_First
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Prepared          : Preparation;
         Sample            : Sample_Nodes.Sample;
         U                 : Dimensionless) is
      begin
         Lemma_Start (Warp, Cache, Denominator_Scale, Prepared, R (Sample.Node), Sample_Nodes.Is_Accurate (Sample), U);
         Lemma_Weight_Start (Prepared, X_Axis, U);
         Lemma_Weight_Start (Prepared, Y_Axis, U);
         Lemma_Weight_Start (Prepared, Z_Axis, U);
         Lemma_Weight_Start (Prepared, E_Axis, U);
         pragma Assert (R (16.0) = 16.0);
         pragma Assert (To_Real (0) = 0.0);
         pragma Assert (Denominator_Scale <= Largest_Partial_Product);
      end Lemma_First;

      procedure Lemma_Pole
        (Warp     : Dimensionless;
         Sample   : Sample_Nodes.Sample;
         Cache    : Rational_Antiderivative;
         Prepared : Preparation;
         Index    : Rational_Degree_Slot;
         U, Ratio : Dimensionless)
      is
         pragma Unreferenced (Warp, Sample, Ratio);
      begin
         Lemma_Term_Kind (Cache, Prepared, Index);
         pragma Assert (U >= 0.0 and then U <= 1.0);
         Float_Lemmas.Lemma_Rounding_Error_Sub (U, Prepared.Terms (Index).Real_Part);
      end Lemma_Pole;

      procedure Lemma_Real_Reach (U : Dimensionless; Term : Pole_Term; X : Dimensionless) is
      begin
         Lemma_Real_Pole_Term (U, Term, X);
      end Lemma_Real_Reach;

      procedure Lemma_Pair_Reach (U : Dimensionless; Term : Pole_Term; X : Dimensionless) is
      begin
         Lemma_Pair_Square_Term (U, Term, X);
         Lemma_Pair_Weights
           (X,
            Term.Imaginary_Part,
            X * X + Term.Imaginary_Part * Term.Imaginary_Part,
            1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part),
            X * (1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part)),
            Term.Imaginary_Part * (1.0 / (X * X + Term.Imaginary_Part * Term.Imaginary_Part)),
            R (U) - R (Term.Real_Part));
      end Lemma_Pair_Reach;

      procedure Lemma_Idle
        (Prepared  : Preparation;
         Index     : Rational_Degree_Slot;
         Axis      : Axis_Name;
         U         : Dimensionless;
         Ret, Size : Dimensionless;
         M         : Real) is
      begin
         Lemma_Idle_Axis (Prepared, Index, Axis, U, Ret, Size, M);
      end Lemma_Idle;

      procedure Lemma_Real_Sum
        (Prepared  : Preparation;
         Index     : Rational_Degree_Slot;
         Axis      : Axis_Name;
         U, X, Rec : Dimensionless;
         Ret, Size : Dimensionless;
         M         : Real)
      is
         pragma Unreferenced (X);
         Residue : constant Dimensionless := Prepared.Terms (Index).Residue_Real (Axis);
      begin
         pragma Assert (abs (R (U) - R (Prepared.Terms (Index).Real_Part)) >= Reach);
         pragma Assert (R (U) - R (Prepared.Terms (Index).Real_Part) /= 0.0);
         Lemma_Real_Axis (Prepared, Index, Axis, U, Rec, Ret, Size, M);
         Lemma_Scale (0.0, abs (R (Residue)), abs (R (Rec)));
         Lemma_Real_Weight_Step (Prepared, Index, Axis, U, M, abs (R (Residue)) * abs (R (Rec)));
         Lemma_Axis_State_Bounds
           (Prepared,
            Index,
            Axis,
            U,
            Ret + Residue * Rec,
            Size + abs Residue * abs Rec,
            M + abs (R (Residue)) * abs (R (Rec)));
      end Lemma_Real_Sum;

      procedure Lemma_Pair_Sum
        (Prepared              : Preparation;
         Index                 : Rational_Degree_Slot;
         Axis                  : Axis_Name;
         U, X, Sq, Inv, Wr, Wi : Dimensionless;
         Ret, Size             : Dimensionless;
         M                     : Real)
      is
         pragma Unreferenced (X, Sq);
         Rr : constant Dimensionless := Prepared.Terms (Index).Residue_Real (Axis);
         Ri : constant Dimensionless := Prepared.Terms (Index).Residue_Imaginary (Axis);
      begin
         Lemma_Pair_Axis (Prepared, Index, Axis, U, Wr, Wi, Ret, Size, R (Inv), M);
         Lemma_Pair_Weight_Step (Prepared, Index, Axis, U, Wr, Wi, R (Inv), M);
         pragma Assert (abs (Rr * Wr - Ri * Wi) <= 2.0**605);
         pragma Assert (abs (abs Rr * abs Wr + abs Ri * Wi) <= 2.0**605);
         Lemma_Axis_State_Bounds
           (Prepared,
            Index,
            Axis,
            U,
            Ret + (Rr * Wr - Ri * Wi),
            Size + (abs Rr * abs Wr + abs Ri * Wi),
            M + (abs (R (Rr)) * abs (R (Wr)) + abs (R (Ri)) * R (Wi)));
      end Lemma_Pair_Sum;

      procedure Lemma_Real_Multiply
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Prepared          : Preparation;
         Sample            : Sample_Nodes.Sample;
         Index             : Rational_Degree_Slot;
         U, Ratio, X, Rec  : Dimensionless;
         Q, Amp            : Dimensionless) is
      begin
         Lemma_Term_Kind (Cache, Prepared, Index);
         pragma Assert (Index <= Cache.Real_Pole_Count);
         Lemma_Real_Factor
           (Warp,
            Cache,
            Denominator_Scale,
            Prepared,
            Sample.Node,
            Sample_Nodes.Is_Accurate (Sample),
            Index,
            U,
            Ratio,
            X,
            Rec,
            Q,
            Amp);
         pragma Assert (abs (Amp + Dimensionless'Max (1.0, abs Rec)) <= 2.0**207);
      end Lemma_Real_Multiply;

      procedure Lemma_Pair_Multiply
        (Warp               : Dimensionless;
         Cache              : Rational_Antiderivative;
         Denominator_Scale  : Dimensionless;
         Prepared           : Preparation;
         Sample             : Sample_Nodes.Sample;
         Index              : Rational_Degree_Slot;
         U, Ratio           : Dimensionless;
         X, Sq, Inv, Wr, Wi : Dimensionless;
         Q, Amp             : Dimensionless)
      is
         pragma Unreferenced (X, Wr, Wi);
      begin
         Lemma_Term_Kind (Cache, Prepared, Index);
         pragma Assert (Index > Cache.Real_Pole_Count);
         Lemma_Pair_Factor
           (Warp,
            Cache,
            Denominator_Scale,
            Prepared,
            Sample.Node,
            Sample_Nodes.Is_Accurate (Sample),
            Index,
            U,
            Ratio,
            Sq,
            Inv,
            Q,
            Amp);
         pragma Assert (abs (Amp + Dimensionless'Max (1.0, Inv)) <= 2.0**207);
      end Lemma_Pair_Multiply;

      procedure Lemma_Last
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Prepared          : Preparation;
         Sample            : Sample_Nodes.Sample;
         U, Ratio          : Dimensionless;
         Retained          : Axis_Values;
         Retained_Size     : Axis_Values;
         Cached_Q          : Dimensionless;
         Amplification     : Dimensionless;
         Magnitude         : Axis_Reals)
      is
         Node : constant Retained_Node :=
           (Good          => True,
            Retained      => Retained,
            Retained_Size => Retained_Size,
            Cached_Q      => Cached_Q,
            Amplification => Amplification);
      begin
         pragma
           Assert
             (for all Axis in Axis_Name =>
                Is_Axis_State
                  (Prepared, Prepared.Count, Axis, U, Retained (Axis), Retained_Size (Axis), Magnitude (Axis)));
         pragma
           Assert (for all Axis in Axis_Name => Is_Weight_State (Prepared, Prepared.Count, Axis, U, Magnitude (Axis)));
         Lemma_Finish (Warp, Cache, Denominator_Scale, Prepared, Sample, U, Ratio, Node, Magnitude);
         Lemma_Signs (Cached_Q, Amplification);
         if Sample_Nodes.Is_Accurate (Sample) and then Amplification <= Largest_Amplification then
            pragma Assert (Is_Node_Accurate (Warp, Cache, Denominator_Scale, Prepared, Node, R (Sample.Node)));
         end if;
      end Lemma_Last;

      procedure Lemma_Result
        (Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Prepared          : Preparation;
         Sample            : Sample_Nodes.Sample;
         Retained          : Axis_Values;
         Retained_Size     : Axis_Values;
         Cached_Q          : Dimensionless;
         Amplification     : Dimensionless)
      is null;

   end Steps;

   ---------------------------------------------------------------------------
   --  The node loop
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  The result
   ---------------------------------------------------------------------------

   --  The loop over the poles returns from two places. A postcondition is one check over every return, in which
   --  the provers know the returned record only case by case, and with a floating-point division in view they
   --  take seconds to do anything case by case. So the loop is a function of its own, Over_Poles, which is given
   --  the computed distance and ratio and never sees the divisions that make them; and its postcondition is
   --  Is_Result, the postcondition of At_Node as a function with a body, which the provers do not look into.
   --  Each return establishes Is_Result for its own record by a lemma, and the postcondition of At_Node follows
   --  from it by its definition alone.

   function Is_Result
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample;
      Node              : Retained_Node) return Boolean
   with
     Ghost,
     Pre  => Has_Layout (Cache),
     Post =>
       Is_Result'Result
       = ((if Node.Good
           then
             Node.Cached_Q > 0.0
             and then
               (if Prepared.Count > 0 or else Denominator_Scale >= Smallest_Partial_Product
                then Node.Cached_Q >= Smallest_Partial_Product)
             and then Node.Cached_Q <= Largest_Partial_Product
             and then Node.Amplification >= 16.0
             and then Node.Amplification <= Largest_Amplification_Value
             and then
               (for all Axis in Axis_Name =>
                  abs Node.Retained (Axis) <= Largest_Retained_Value
                  and then Node.Retained_Size (Axis) >= 0.0
                  and then Node.Retained_Size (Axis) <= Largest_Retained_Value))
          and then
            (if Node.Good
               and then Sample_Nodes.Is_Accurate (Sample)
               and then Node.Amplification <= Largest_Amplification
             then Is_Node_Accurate (Warp, Cache, Denominator_Scale, Prepared, Node, R (Sample.Node))));
   --  The postcondition of At_Node, of Node.

   function Is_Result
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample;
      Node              : Retained_Node) return Boolean is
   begin
      return
        (if Node.Good
         then
           Node.Cached_Q > 0.0
           and then
             (if Prepared.Count > 0 or else Denominator_Scale >= Smallest_Partial_Product
              then Node.Cached_Q >= Smallest_Partial_Product)
           and then Node.Cached_Q <= Largest_Partial_Product
           and then Node.Amplification >= 16.0
           and then Node.Amplification <= Largest_Amplification_Value
           and then
             (for all Axis in Axis_Name =>
                abs Node.Retained (Axis) <= Largest_Retained_Value
                and then Node.Retained_Size (Axis) >= 0.0
                and then Node.Retained_Size (Axis) <= Largest_Retained_Value))
        and then
          (if Node.Good and then Sample_Nodes.Is_Accurate (Sample) and then Node.Amplification <= Largest_Amplification
           then Is_Node_Accurate (Warp, Cache, Denominator_Scale, Prepared, Node, R (Sample.Node)));
   end Is_Result;

   procedure Lemma_Refused_Result
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample)
   with
     Ghost,
     Pre  => Has_Layout (Cache),
     Post => Is_Result (Warp, Cache, Denominator_Scale, Prepared, Sample, (Good => False, others => <>));
   --  Nothing is stated of a node which is not good.

   procedure Lemma_Refused_Result
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample)
   is
      Node : constant Retained_Node := (Good => False, others => <>);
   begin
      pragma Assert (not Node.Good);
      pragma Assert (Is_Result (Warp, Cache, Denominator_Scale, Prepared, Sample, Node));
   end Lemma_Refused_Result;

   procedure Lemma_Good_Result
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample;
      Retained          : Axis_Values;
      Retained_Size     : Axis_Values;
      Cached_Q          : Dimensionless;
      Amplification     : Dimensionless)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then
         Steps.Is_Finished
           (Warp, Cache, Denominator_Scale, Prepared, Sample, Retained, Retained_Size, Cached_Q, Amplification)
       and then (Prepared.Count = 0 or else Cached_Q >= Smallest_Partial_Product),
     Post =>
       Is_Result
         (Warp,
          Cache,
          Denominator_Scale,
          Prepared,
          Sample,
          (Good          => True,
           Retained      => Retained,
           Retained_Size => Retained_Size,
           Cached_Q      => Cached_Q,
           Amplification => Amplification));
   --  What the loop has maintained gives the postcondition of At_Node for the record made of its variables.

   procedure Lemma_Good_Result
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample;
      Retained          : Axis_Values;
      Retained_Size     : Axis_Values;
      Cached_Q          : Dimensionless;
      Amplification     : Dimensionless)
   is
      Node : constant Retained_Node :=
        (Good          => True,
         Retained      => Retained,
         Retained_Size => Retained_Size,
         Cached_Q      => Cached_Q,
         Amplification => Amplification);
   begin
      Steps.Lemma_Result
        (Warp, Cache, Denominator_Scale, Prepared, Sample, Retained, Retained_Size, Cached_Q, Amplification);
      pragma Assert (Node.Good);
      pragma Assert (Node.Cached_Q > 0.0 and then Node.Cached_Q <= Largest_Partial_Product);
      pragma
        Assert
          (if Prepared.Count > 0 or else Denominator_Scale >= Smallest_Partial_Product
           then Node.Cached_Q >= Smallest_Partial_Product);
      pragma Assert (Node.Amplification >= 16.0 and then Node.Amplification <= Largest_Amplification_Value);
      pragma
        Assert
          (for all Axis in Axis_Name =>
             abs Node.Retained (Axis) <= Largest_Retained_Value
             and then Node.Retained_Size (Axis) >= 0.0
             and then Node.Retained_Size (Axis) <= Largest_Retained_Value);
      pragma
        Assert
          (if Sample_Nodes.Is_Accurate (Sample) and then Node.Amplification <= Largest_Amplification
           then Is_Node_Accurate (Warp, Cache, Denominator_Scale, Prepared, Node, R (Sample.Node)));
      pragma Assert (Is_Result (Warp, Cache, Denominator_Scale, Prepared, Sample, Node));
   end Lemma_Good_Result;

   function Over_Poles
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample;
      U, Ratio          : Dimensionless) return Retained_Node
   with
     Inline,
     Pre  =>
       Prepared.Outcome = Certified
       and then Denominator_Scale > 0.0
       and then Denominator_Scale <= Magnitude_Limit
       and then Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then Poles_Are_Off_The_Curve (Cache)
       and then Poles.Is_Cache_Prepared (Cache, Prepared)
       and then Sample_Nodes.Is_In_Range (Sample)
       and then Steps.Is_Set_Up (Warp, Sample, U, Ratio),
     Post => Is_Result (Warp, Cache, Denominator_Scale, Prepared, Sample, Over_Poles'Result);
   --  At_Node from its loop on: U is the computed distance of the sample and Ratio the computed h/W.

   function Over_Poles
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample;
      U, Ratio          : Dimensionless) return Retained_Node
   is
      Cached_Q      : Dimensionless := Denominator_Scale;
      Amplification : Dimensionless := 16.0;
      Retained      : Axis_Values := [others => 0.0];
      Retained_Size : Axis_Values := [others => 0.0];

      Magnitude : Axis_Reals := [others => 0.0]
      with Ghost;
      --  The exact sum of the magnitudes of the computed terms, which Retained_Size approximates.
   begin
      Steps.Lemma_First (Warp, Cache, Denominator_Scale, Prepared, Sample, U);
      for Index in 1 .. Prepared.Count loop
         pragma Loop_Invariant (Index = 1 or else Cached_Q >= Smallest_Partial_Product);
         pragma
           Loop_Invariant
             (Steps.Is_Axis_Sound
                (Prepared, Index - 1, X_Axis, U, Retained (X_Axis), Retained_Size (X_Axis), Magnitude (X_Axis))
              and then
                Steps.Is_Axis_Sound
                  (Prepared, Index - 1, Y_Axis, U, Retained (Y_Axis), Retained_Size (Y_Axis), Magnitude (Y_Axis))
              and then
                Steps.Is_Axis_Sound
                  (Prepared, Index - 1, Z_Axis, U, Retained (Z_Axis), Retained_Size (Z_Axis), Magnitude (Z_Axis))
              and then
                Steps.Is_Axis_Sound
                  (Prepared, Index - 1, E_Axis, U, Retained (E_Axis), Retained_Size (E_Axis), Magnitude (E_Axis)));
         pragma
           Loop_Invariant
             (Steps.Is_Product_Sound (Warp, Cache, Denominator_Scale, Sample, Index - 1, Cached_Q, Amplification));
         Steps.Lemma_Pole (Warp, Sample, Cache, Prepared, Index, U, Ratio);
         declare
            Term : Pole_Term renames Prepared.Terms (Index);
            X    : constant Dimensionless := U - Term.Real_Part;
         begin
            if Term.Imaginary_Part = 0.0 then
               Steps.Lemma_Real_Reach (U, Term, X);
               declare
                  Reciprocal : constant Dimensionless := 1.0 / X;
               begin
                  for Axis in Axis_Name loop
                     Steps.Lemma_Idle
                       (Prepared, Index, Axis, U, Retained (Axis), Retained_Size (Axis), Magnitude (Axis));
                     if Prepared.Active (Axis) then
                        Steps.Lemma_Real_Sum
                          (Prepared,
                           Index,
                           Axis,
                           U,
                           X,
                           Reciprocal,
                           Retained (Axis),
                           Retained_Size (Axis),
                           Magnitude (Axis));
                        Retained (Axis) := Retained (Axis) + Term.Residue_Real (Axis) * Reciprocal;
                        Retained_Size (Axis) := Retained_Size (Axis) + abs Term.Residue_Real (Axis) * abs Reciprocal;
                        Magnitude (Axis) :=
                          Steps.Real_Magnitude (Magnitude (Axis), Term.Residue_Real (Axis), Reciprocal);
                        pragma
                          Assert
                            (Steps.Is_Axis_Sound
                               (Prepared,
                                Index,
                                X_Axis,
                                U,
                                Retained (X_Axis),
                                Retained_Size (X_Axis),
                                Magnitude (X_Axis))
                             and then
                               (if Y_Axis <= Axis
                                then
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index,
                                     Y_Axis,
                                     U,
                                     Retained (Y_Axis),
                                     Retained_Size (Y_Axis),
                                     Magnitude (Y_Axis))
                                else
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index - 1,
                                     Y_Axis,
                                     U,
                                     Retained (Y_Axis),
                                     Retained_Size (Y_Axis),
                                     Magnitude (Y_Axis)))
                             and then
                               (if Z_Axis <= Axis
                                then
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index,
                                     Z_Axis,
                                     U,
                                     Retained (Z_Axis),
                                     Retained_Size (Z_Axis),
                                     Magnitude (Z_Axis))
                                else
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index - 1,
                                     Z_Axis,
                                     U,
                                     Retained (Z_Axis),
                                     Retained_Size (Z_Axis),
                                     Magnitude (Z_Axis)))
                             and then
                               (if E_Axis <= Axis
                                then
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index,
                                     E_Axis,
                                     U,
                                     Retained (E_Axis),
                                     Retained_Size (E_Axis),
                                     Magnitude (E_Axis))
                                else
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index - 1,
                                     E_Axis,
                                     U,
                                     Retained (E_Axis),
                                     Retained_Size (E_Axis),
                                     Magnitude (E_Axis))));
                     end if;
                     pragma
                       Loop_Invariant
                         (Steps.Is_Axis_Sound
                            (Prepared, Index, X_Axis, U, Retained (X_Axis), Retained_Size (X_Axis), Magnitude (X_Axis))
                          and then
                            (if Y_Axis <= Axis
                             then
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index,
                                  Y_Axis,
                                  U,
                                  Retained (Y_Axis),
                                  Retained_Size (Y_Axis),
                                  Magnitude (Y_Axis))
                             else
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index - 1,
                                  Y_Axis,
                                  U,
                                  Retained (Y_Axis),
                                  Retained_Size (Y_Axis),
                                  Magnitude (Y_Axis)))
                          and then
                            (if Z_Axis <= Axis
                             then
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index,
                                  Z_Axis,
                                  U,
                                  Retained (Z_Axis),
                                  Retained_Size (Z_Axis),
                                  Magnitude (Z_Axis))
                             else
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index - 1,
                                  Z_Axis,
                                  U,
                                  Retained (Z_Axis),
                                  Retained_Size (Z_Axis),
                                  Magnitude (Z_Axis)))
                          and then
                            (if E_Axis <= Axis
                             then
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index,
                                  E_Axis,
                                  U,
                                  Retained (E_Axis),
                                  Retained_Size (E_Axis),
                                  Magnitude (E_Axis))
                             else
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index - 1,
                                  E_Axis,
                                  U,
                                  Retained (E_Axis),
                                  Retained_Size (E_Axis),
                                  Magnitude (E_Axis))));
                  end loop;
                  Steps.Lemma_Real_Multiply
                    (Warp,
                     Cache,
                     Denominator_Scale,
                     Prepared,
                     Sample,
                     Index,
                     U,
                     Ratio,
                     X,
                     Reciprocal,
                     Cached_Q,
                     Amplification);
                  --  (h/W)·(p - U)/p, which is positive because p lies outside [0, 1].
                  Cached_Q := Cached_Q * (Ratio * (-X * Term.Reciprocal_Norm));
                  Amplification := Amplification + Dimensionless'Max (1.0, abs Reciprocal);
               end;
            else
               Steps.Lemma_Pair_Reach (U, Term, X);
               declare
                  Square  : constant Dimensionless := X * X + Term.Imaginary_Part * Term.Imaginary_Part;
                  Inverse : constant Dimensionless := 1.0 / Square;
                  W_Real  : constant Dimensionless := X * Inverse;
                  W_Imag  : constant Dimensionless := Term.Imaginary_Part * Inverse;
               begin
                  for Axis in Axis_Name loop
                     Steps.Lemma_Idle
                       (Prepared, Index, Axis, U, Retained (Axis), Retained_Size (Axis), Magnitude (Axis));
                     if Prepared.Active (Axis) then
                        Steps.Lemma_Pair_Sum
                          (Prepared,
                           Index,
                           Axis,
                           U,
                           X,
                           Square,
                           Inverse,
                           W_Real,
                           W_Imag,
                           Retained (Axis),
                           Retained_Size (Axis),
                           Magnitude (Axis));
                        Retained (Axis) :=
                          Retained (Axis)
                          + (Term.Residue_Real (Axis) * W_Real - Term.Residue_Imaginary (Axis) * W_Imag);
                        Retained_Size (Axis) :=
                          Retained_Size (Axis)
                          + (abs Term.Residue_Real (Axis) * abs W_Real + abs Term.Residue_Imaginary (Axis) * W_Imag);
                        Magnitude (Axis) :=
                          Steps.Pair_Magnitude
                            (Magnitude (Axis),
                             Term.Residue_Real (Axis),
                             Term.Residue_Imaginary (Axis),
                             W_Real,
                             W_Imag);
                        pragma
                          Assert
                            (Steps.Is_Axis_Sound
                               (Prepared,
                                Index,
                                X_Axis,
                                U,
                                Retained (X_Axis),
                                Retained_Size (X_Axis),
                                Magnitude (X_Axis))
                             and then
                               (if Y_Axis <= Axis
                                then
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index,
                                     Y_Axis,
                                     U,
                                     Retained (Y_Axis),
                                     Retained_Size (Y_Axis),
                                     Magnitude (Y_Axis))
                                else
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index - 1,
                                     Y_Axis,
                                     U,
                                     Retained (Y_Axis),
                                     Retained_Size (Y_Axis),
                                     Magnitude (Y_Axis)))
                             and then
                               (if Z_Axis <= Axis
                                then
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index,
                                     Z_Axis,
                                     U,
                                     Retained (Z_Axis),
                                     Retained_Size (Z_Axis),
                                     Magnitude (Z_Axis))
                                else
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index - 1,
                                     Z_Axis,
                                     U,
                                     Retained (Z_Axis),
                                     Retained_Size (Z_Axis),
                                     Magnitude (Z_Axis)))
                             and then
                               (if E_Axis <= Axis
                                then
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index,
                                     E_Axis,
                                     U,
                                     Retained (E_Axis),
                                     Retained_Size (E_Axis),
                                     Magnitude (E_Axis))
                                else
                                  Steps.Is_Axis_Sound
                                    (Prepared,
                                     Index - 1,
                                     E_Axis,
                                     U,
                                     Retained (E_Axis),
                                     Retained_Size (E_Axis),
                                     Magnitude (E_Axis))));
                     end if;
                     pragma
                       Loop_Invariant
                         (Steps.Is_Axis_Sound
                            (Prepared, Index, X_Axis, U, Retained (X_Axis), Retained_Size (X_Axis), Magnitude (X_Axis))
                          and then
                            (if Y_Axis <= Axis
                             then
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index,
                                  Y_Axis,
                                  U,
                                  Retained (Y_Axis),
                                  Retained_Size (Y_Axis),
                                  Magnitude (Y_Axis))
                             else
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index - 1,
                                  Y_Axis,
                                  U,
                                  Retained (Y_Axis),
                                  Retained_Size (Y_Axis),
                                  Magnitude (Y_Axis)))
                          and then
                            (if Z_Axis <= Axis
                             then
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index,
                                  Z_Axis,
                                  U,
                                  Retained (Z_Axis),
                                  Retained_Size (Z_Axis),
                                  Magnitude (Z_Axis))
                             else
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index - 1,
                                  Z_Axis,
                                  U,
                                  Retained (Z_Axis),
                                  Retained_Size (Z_Axis),
                                  Magnitude (Z_Axis)))
                          and then
                            (if E_Axis <= Axis
                             then
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index,
                                  E_Axis,
                                  U,
                                  Retained (E_Axis),
                                  Retained_Size (E_Axis),
                                  Magnitude (E_Axis))
                             else
                               Steps.Is_Axis_Sound
                                 (Prepared,
                                  Index - 1,
                                  E_Axis,
                                  U,
                                  Retained (E_Axis),
                                  Retained_Size (E_Axis),
                                  Magnitude (E_Axis))));
                  end loop;
                  Steps.Lemma_Pair_Multiply
                    (Warp,
                     Cache,
                     Denominator_Scale,
                     Prepared,
                     Sample,
                     Index,
                     U,
                     Ratio,
                     X,
                     Square,
                     Inverse,
                     W_Real,
                     W_Imag,
                     Cached_Q,
                     Amplification);
                  Cached_Q := Cached_Q * ((Ratio * Ratio) * (Square * Term.Reciprocal_Norm));
                  Amplification := Amplification + Dimensionless'Max (1.0, Inverse);
               end;
            end if;
            pragma
              Assert_And_Cut
                (Steps.Is_Axis_Sound
                   (Prepared, Index, X_Axis, U, Retained (X_Axis), Retained_Size (X_Axis), Magnitude (X_Axis))
                 and then
                   Steps.Is_Axis_Sound
                     (Prepared, Index, Y_Axis, U, Retained (Y_Axis), Retained_Size (Y_Axis), Magnitude (Y_Axis))
                 and then
                   Steps.Is_Axis_Sound
                     (Prepared, Index, Z_Axis, U, Retained (Z_Axis), Retained_Size (Z_Axis), Magnitude (Z_Axis))
                 and then
                   Steps.Is_Axis_Sound
                     (Prepared, Index, E_Axis, U, Retained (E_Axis), Retained_Size (E_Axis), Magnitude (E_Axis))
                 and then
                   (if Cached_Q >= Smallest_Partial_Product and then Cached_Q <= Largest_Partial_Product
                    then
                      Steps.Is_Product_Sound
                        (Warp, Cache, Denominator_Scale, Sample, Index, Cached_Q, Amplification)));
            --  From here on only this is known of what the pole has changed: what is proved below, the
            --  postcondition at the return among it, does not have its many operations to look through.

            --  Nothing above can overflow or lose its relative accuracy while the running product stays within
            --  these limits. The finished product must be near the ideal denominator, so a product which leaves
            --  them belongs to a cache which the certificate would refuse in any case.
            if not (Cached_Q >= Smallest_Partial_Product and then Cached_Q <= Largest_Partial_Product) then
               Lemma_Refused_Result (Warp, Cache, Denominator_Scale, Prepared, Sample);
               return (Good => False, others => <>);
            end if;
         end;
         pragma Assert (Cached_Q >= Smallest_Partial_Product);
         pragma
           Assert
             (Steps.Is_Axis_Sound
                (Prepared, Index, X_Axis, U, Retained (X_Axis), Retained_Size (X_Axis), Magnitude (X_Axis))
              and then
                Steps.Is_Axis_Sound
                  (Prepared, Index, Y_Axis, U, Retained (Y_Axis), Retained_Size (Y_Axis), Magnitude (Y_Axis))
              and then
                Steps.Is_Axis_Sound
                  (Prepared, Index, Z_Axis, U, Retained (Z_Axis), Retained_Size (Z_Axis), Magnitude (Z_Axis))
              and then
                Steps.Is_Axis_Sound
                  (Prepared, Index, E_Axis, U, Retained (E_Axis), Retained_Size (E_Axis), Magnitude (E_Axis)));
         pragma
           Assert (Steps.Is_Product_Sound (Warp, Cache, Denominator_Scale, Sample, Index, Cached_Q, Amplification));
      end loop;
      Steps.Lemma_Last
        (Warp,
         Cache,
         Denominator_Scale,
         Prepared,
         Sample,
         U,
         Ratio,
         Retained,
         Retained_Size,
         Cached_Q,
         Amplification,
         Magnitude);
      pragma
        Assert_And_Cut
          (Steps.Is_Finished
             (Warp, Cache, Denominator_Scale, Prepared, Sample, Retained, Retained_Size, Cached_Q, Amplification)
           and then (Prepared.Count = 0 or else Cached_Q >= Smallest_Partial_Product));
      Lemma_Good_Result
        (Warp, Cache, Denominator_Scale, Prepared, Sample, Retained, Retained_Size, Cached_Q, Amplification);
      return
        (Good          => True,
         Retained      => Retained,
         Retained_Size => Retained_Size,
         Cached_Q      => Cached_Q,
         Amplification => Amplification);
   end Over_Poles;

   function At_Node
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample) return Retained_Node
   with Refined_Post => Is_Result (Warp, Cache, Denominator_Scale, Prepared, Sample, At_Node'Result)
   is
      Finite : constant Boolean := Steps.Setup_Is_Finite (Warp, Sample)
      with Ghost;
      --  Nothing overflows in the three declarations which follow.

      H     : constant Dimensionless := Warp * Sample.Complement + Sample.Node;
      U     : constant Dimensionless := Dimensionless'Min (1.0, Sample.Node / H);
      Ratio : constant Dimensionless := H / Warp;
   begin
      pragma Assert (Finite);
      Steps.Lemma_Distance (Warp, Sample);
      return Over_Poles (Warp, Cache, Denominator_Scale, Prepared, Sample, U, Ratio);
   end At_Node;

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Retained_Side;
