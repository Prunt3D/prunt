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

pragma Warnings ("F");
--  A lemma can take a parameter only for its contract, which is not compiled.

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile. The
--  ghost code among them computes with exact rationals and would be far too slow to run.

with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Ideal_Side;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Retained_Side;

package body Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Nodes
  with SPARK_Mode => On
is

   --  How the two nodal bounds cover what they bound. Write u for Epsilon, hats for computed values, and
   --
   --     A = Q̂·(1 + FE),   B = D̂ + IA,   X = |Î - Ĉ| + Slip + 8u·Size + |Î|·(IA + 8u) + 64u·Ĉsize + 8u·Slope
   --                                        + 7u·(|Î| + |Ĉ|)
   --
   --  for the exact values of what the code computes as Scale = A·B and as the bracket of Nodal_G.
   --
   --  Every rounded sum or product of nonnegative operands is at least (1 - u) times the exact one, less Eta. So
   --
   --     Nodal_E ≥ (1 + 2⁻⁴¹)·(|Q̂ - D̂| + Q̂·FE + IA) + 2⁻⁵⁰¹,
   --     Nodal_G ≥ (1 + 2⁻⁴¹)·A·B·X + 3·2⁻⁵⁰²:
   --
   --  nine and about twenty roundings lose at most 2⁻⁴⁸ of the 2⁻⁴⁰ which Rounding_Factor adds, the term
   --  8u·(Q̂ + D̂) of Nodal_E and one u·(|Î| + |Ĉ|) of the bracket pay for the rounding of the two differences,
   --  and what is left of Underflow_Padding exceeds every Eta, amplified by a Scale of at most 1.2·10⁶⁰.
   --
   --  The stages give |Q - Q̂| ≤ 2⁻⁴⁷·Amp·Q̂ ≤ Q̂·FE and |D - D̂| ≤ IA·(1 + Slack) + Denominator_Dust, which
   --  bound e = Q - D by the first line. For g = Q·D·(T - R): |Q| ≤ A; D ≤ B·(1 + Slack + Denominator_Dust),
   --  because B ≥ 1 makes the dust a relative error; and |T - R| ≤ (1 + Slack)·X + 2⁻⁷⁹⁹ from the allowances of
   --  the two tangents, the rounding of Ĉ and of Î - Ĉ. The product is at most (1 + 2⁻⁴²)·A·B·X + 2⁻⁵⁹⁸,
   --  which the second line exceeds.

   ---------------------------------------------------------------------------
   --  Constants of the analysis
   ---------------------------------------------------------------------------

   Term_Allowance_Real   : constant := 2.0**(-47);
   Factor_Allowance_Real : constant := 2.0**(-45);
   Rounding_Factor_Real  : constant := 1.0 + 2.0**(-40);
   Padding               : constant := 2.0**(-500);
   --  Term_Allowance, Factor_Allowance, Rounding_Factor and Underflow_Padding as real numbers.

   Normal_Floor : constant := 2.0**(-1022);
   --  Eta / Epsilon. An exact result at least this large absorbs Eta into one more Epsilon of relative error.

   Seven_Epsilon : constant := 7.0 * 2.0**(-53);
   --  What is left of Small_Allowance for the magnitudes of the two tangents, after one Epsilon has paid for
   --  the rounding of their difference.

   E_Factor     : constant := 1.0 + 2.0**(-41);
   Half_Padding : constant := 2.0**(-501);
   --  What is left of Rounding_Factor and of Underflow_Padding after the rounding of Nodal_E.

   Scale_Factor : constant := 1.0 - 8.0 * 2.0**(-53);
   --  The computed Scale is at least this times the exact one: four operations, with Eta absorbed in each.

   Bracket_Factor : constant := 1.0 - 9.0 * 2.0**(-53);
   Bracket_Dust   : constant := 2.0**(-1071);
   --  The computed bracket of Nodal_G is at least Bracket_Factor times the exact one, less Bracket_Dust.

   Product_Factor : constant := 1.0 + 2.0**(-40) - 2.0**(-50);
   Padding_Part   : constant := 7.0 * 2.0**(-503);
   --  What is left of Rounding_Factor and of Underflow_Padding after the last three operations of Nodal_G.

   G_Factor     : constant := 1.0 + 2.0**(-41);
   Padding_Left : constant := 3.0 * 2.0**(-502);
   --  What is left of them after the rounding of Scale and of the bracket as well.

   Gap_Dust : constant := 2.0**(-799);
   --  More than Tangent_Dust + Retained_Dust + 3 * Eta.

   Denominator_Factor : constant := 1.0 + 2.0**(-44) + 2.0**(-1010);
   --  1 + Slack + Denominator_Dust.

   Scale_Bound : constant := 1.2E60;
   --  No Scale is larger, computed or exact.

   Scale_Limit   : constant Dimensionless := 1.1E60
   with Ghost;
   Cached_Limit  : constant Dimensionless := 1.1E185
   with Ghost;
   Bracket_Limit : constant Dimensionless := 1.2E185
   with Ghost;
   Product_Limit : constant Dimensionless := 1.4E245
   with Ghost;
   Padded_Limit  : constant Dimensionless := 1.5E245
   with Ghost;
   --  Limits of the intermediate values of Nodal_G.

   Narrow_Limit : constant Dimensionless := 2.0**200
   with Ghost;
   Wide_Limit   : constant Dimensionless := 2.0**820
   with Ghost;
   --  Limits of the two operands of a product whose rounding is analysed here. Float_Lemmas would need both
   --  within 2**511, which the bracket of Nodal_G is not.

   Sum_Limit : constant Dimensionless := 2.0**1000
   with Ghost;
   --  Limit of the operands of a sum whose rounding is analysed here.

   Narrow_Bound : constant := 2.0**200;
   Wide_Bound   : constant := 2.0**820;
   Sum_Bound    : constant := 2.0**1000;
   --  The same three limits as real numbers.

   Sum_Top     : constant Dimensionless := 2.0**1001
   with Ghost;
   Product_Top : constant Dimensionless := 2.0**1020
   with Ghost;
   --  Limits of such a sum and of such a product, rounding included.

   Factor_Error_Limit : constant Dimensionless := 1.0E50
   with Ghost;
   --  No Factor_Error is larger, accepted or not.

   E_Limit : constant Dimensionless := 1.0E101
   with Ghost;
   --  Limit of the intermediate values of Nodal_E.

   ---------------------------------------------------------------------------
   --  Single rounded operations
   ---------------------------------------------------------------------------

   procedure Lemma_Abs_Is_Exact (Value : Dimensionless)
   with Ghost, Post => R (abs Value) = abs R (Value);
   --  The floating-point absolute value is not rounded.

   procedure Lemma_Abs_Is_Exact (Value : Dimensionless) is null;

   procedure Lemma_Constants
   with
     Ghost,
     Post =>
       R (Small_Allowance) = Small_Allowance_Real
       and then R (Term_Allowance) = Term_Allowance_Real
       and then R (Factor_Allowance) = Factor_Allowance_Real
       and then R (Rounding_Factor) = Rounding_Factor_Real
       and then R (Underflow_Padding) = Padding
       and then R (Dimensionless'(1.0)) = 1.0;
   --  The floating-point constants are the real numbers they are written as.

   procedure Lemma_Constants is null;

   procedure Lemma_Rounding_Error_Mul_Right (Narrow, Wide : Dimensionless)
   with
     Ghost,
     Pre  => Narrow >= 0.0 and then Narrow <= Narrow_Limit and then Wide >= 0.0 and then Wide <= Wide_Limit,
     Post => abs (R (Narrow * Wide) - R (Narrow) * R (Wide)) <= Epsilon * abs (R (Narrow) * R (Wide)) + Eta;
   --  Float_Lemmas.Lemma_Rounding_Error_Mul for operands whose product cannot overflow although one of them may
   --  be larger than the square root of the largest number.

   procedure Lemma_Rounding_Error_Mul_Right (Narrow, Wide : Dimensionless) is null;

   procedure Lemma_Rounding_Error_Mul_Left (Wide, Narrow : Dimensionless)
   with
     Ghost,
     Pre  => Narrow >= 0.0 and then Narrow <= Narrow_Limit and then Wide >= 0.0 and then Wide <= Wide_Limit,
     Post => abs (R (Wide * Narrow) - R (Wide) * R (Narrow)) <= Epsilon * abs (R (Wide) * R (Narrow)) + Eta;
   --  The same with the operands in the other order.

   procedure Lemma_Rounding_Error_Mul_Left (Wide, Narrow : Dimensionless) is null;

   procedure Lemma_Nonnegative_Product (X, Y : Real)
   with Ghost, Pre => X >= 0.0 and then Y >= 0.0, Post => X * Y >= 0.0;
   --  The provers do not always see this unaided where the context is large.

   procedure Lemma_Nonnegative_Product (X, Y : Real) is null;

   procedure Lemma_Limits
   with
     Ghost,
     Post =>
       R (Scale_Limit) >= 1.09E60
       and then R (Scale_Limit) <= 1.101E60
       and then R (Cached_Limit) >= 1.09E185
       and then R (Cached_Limit) <= 1.101E185
       and then R (Bracket_Limit) >= 1.19E185
       and then R (Bracket_Limit) <= 1.201E185
       and then R (Product_Limit) >= 1.39E245
       and then R (Padded_Limit) >= 1.49E245
       and then R (Factor_Error_Limit) >= 9.9E49
       and then R (E_Limit) >= 9.9E100
       and then R (Dimensionless'(2.0)) = 2.0
       and then R (Dimensionless'(1.1E30)) >= 1.09E30;
   --  The floating-point limits against round real numbers.

   procedure Lemma_Limits is null;

   procedure Lemma_Float_Bound (Value, Limit : Dimensionless)
   with Ghost, Pre => R (Value) <= R (Limit), Post => Value <= Limit;
   --  Floating-point numbers compare as the real numbers they denote. The bounds below are derived over the
   --  real numbers, which the provers handle far better than bounds of rounded operations.

   procedure Lemma_Float_Bound (Value, Limit : Dimensionless) is null;

   procedure Lemma_Mul_Bounds_Right (Narrow, Wide : Dimensionless)
   with
     Ghost,
     Pre  => Narrow >= 0.0 and then R (Narrow) <= Narrow_Bound and then Wide >= 0.0 and then R (Wide) <= Wide_Bound,
     Post =>
       Narrow * Wide in 0.0 .. Product_Top
       and then R (Narrow) * R (Wide) >= 0.0
       and then R (Narrow * Wide) >= (1.0 - Epsilon) * (R (Narrow) * R (Wide)) - Eta
       and then R (Narrow * Wide) <= (1.0 + Epsilon) * (R (Narrow) * R (Wide)) + Eta;
   --  A rounded product of nonnegative operands is finite and within Epsilon of the exact one, relatively, and
   --  Eta.

   procedure Lemma_Mul_Bounds_Right (Narrow, Wide : Dimensionless) is
   begin
      Lemma_Float_Bound (Narrow, Narrow_Limit);
      Lemma_Float_Bound (Wide, Wide_Limit);
      Lemma_Rounding_Error_Mul_Right (Narrow, Wide);
      Lemma_Nonnegative_Product (R (Narrow), R (Wide));
   end Lemma_Mul_Bounds_Right;

   procedure Lemma_Mul_Bounds_Left (Wide, Narrow : Dimensionless)
   with
     Ghost,
     Pre  => Narrow >= 0.0 and then R (Narrow) <= Narrow_Bound and then Wide >= 0.0 and then R (Wide) <= Wide_Bound,
     Post =>
       Wide * Narrow in 0.0 .. Product_Top
       and then R (Wide) * R (Narrow) >= 0.0
       and then R (Wide * Narrow) >= (1.0 - Epsilon) * (R (Wide) * R (Narrow)) - Eta
       and then R (Wide * Narrow) <= (1.0 + Epsilon) * (R (Wide) * R (Narrow)) + Eta;

   procedure Lemma_Mul_Bounds_Left (Wide, Narrow : Dimensionless) is
   begin
      Lemma_Float_Bound (Narrow, Narrow_Limit);
      Lemma_Float_Bound (Wide, Wide_Limit);
      Lemma_Rounding_Error_Mul_Left (Wide, Narrow);
      Lemma_Nonnegative_Product (R (Wide), R (Narrow));
   end Lemma_Mul_Bounds_Left;

   procedure Lemma_Add_Bounds (A, B : Dimensionless)
   with
     Ghost,
     Pre  => A >= 0.0 and then R (A) <= Sum_Bound and then B >= 0.0 and then R (B) <= Sum_Bound,
     Post =>
       A + B in 0.0 .. Sum_Top
       and then R (A + B) >= (1.0 - Epsilon) * (R (A) + R (B)) - Eta
       and then R (A + B) <= (1.0 + Epsilon) * (R (A) + R (B)) + Eta;
   --  A rounded sum of nonnegative operands is finite and within Epsilon of the exact one, relatively, and Eta.

   procedure Lemma_Add_Bounds (A, B : Dimensionless) is
   begin
      Lemma_Float_Bound (A, Sum_Limit);
      Lemma_Float_Bound (B, Sum_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Add (A, B);
      pragma Assert (R (A) + R (B) >= 0.0);
   end Lemma_Add_Bounds;

   procedure Lemma_Signed_Add_Bounds (A, B : Dimensionless)
   with
     Ghost,
     Pre  => abs R (A) <= Sum_Bound and then abs R (B) <= Sum_Bound,
     Post =>
       A + B in -Sum_Top .. Sum_Top and then abs (R (A + B) - (R (A) + R (B))) <= Epsilon * abs (R (A) + R (B)) + Eta;
   --  The same for operands of either sign,

   procedure Lemma_Signed_Add_Bounds (A, B : Dimensionless) is
   begin
      Lemma_Abs_Is_Exact (A);
      Lemma_Abs_Is_Exact (B);
      Lemma_Float_Bound (abs A, Sum_Limit);
      Lemma_Float_Bound (abs B, Sum_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Add (A, B);
   end Lemma_Signed_Add_Bounds;

   procedure Lemma_Sub_Bounds (A, B : Dimensionless)
   with
     Ghost,
     Pre  => abs R (A) <= Sum_Bound and then abs R (B) <= Sum_Bound,
     Post =>
       A - B in -Sum_Top .. Sum_Top and then abs (R (A - B) - (R (A) - R (B))) <= Epsilon * abs (R (A) - R (B)) + Eta;
   --  and for a difference.

   procedure Lemma_Sub_Bounds (A, B : Dimensionless) is
   begin
      Lemma_Abs_Is_Exact (A);
      Lemma_Abs_Is_Exact (B);
      Lemma_Float_Bound (abs A, Sum_Limit);
      Lemma_Float_Bound (abs B, Sum_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Sub (A, B);
   end Lemma_Sub_Bounds;

   ---------------------------------------------------------------------------
   --  Real arithmetic
   ---------------------------------------------------------------------------

   procedure Lemma_Relative_Low (Value, Exact : Real)
   with
     Ghost,
     Pre  => Exact >= Normal_Floor and then Value >= (1.0 - Epsilon) * Exact - Eta,
     Post => Value >= (1.0 - 2.0 * Epsilon) * Exact;
   --  Eta is at most Epsilon times an exact result which is not tiny.

   procedure Lemma_Relative_Low (Value, Exact : Real) is
   begin
      pragma Assert (Eta <= Epsilon * Exact);
   end Lemma_Relative_Low;

   procedure Lemma_Scale_Order (Low, High, Factor : Real)
   with
     Ghost,
     Pre  => Low <= High and then Factor >= 0.0,
     Post => Low * Factor <= High * Factor and then Factor * Low <= Factor * High;

   procedure Lemma_Scale_Order (Low, High, Factor : Real) is
   begin
      pragma Assert ((High - Low) * Factor >= 0.0);
      pragma Assert ((High - Low) * Factor = High * Factor - Low * Factor);
   end Lemma_Scale_Order;

   procedure Lemma_Product_Low (A, B, A0, B0, Ca, Cb : Real)
   with
     Ghost,
     Pre  =>
       A0 >= 0.0 and then B0 >= 0.0 and then Ca >= 0.0 and then Cb >= 0.0 and then A >= Ca * A0 and then B >= Cb * B0,
     Post => A * B >= (Ca * Cb) * (A0 * B0) and then A0 * B0 >= 0.0;
   --  Lower bounds of two nonnegative factors multiply.

   procedure Lemma_Product_Low (A, B, A0, B0, Ca, Cb : Real) is
   begin
      Lemma_Nonnegative_Product (Ca, A0);
      Lemma_Nonnegative_Product (Cb, B0);
      Lemma_Nonnegative_Product (A0, B0);
      Lemma_Scale_Order (Ca * A0, A, B);
      Lemma_Scale_Order (Cb * B0, B, Ca * A0);
      pragma Assert (A * B >= (Ca * A0) * (Cb * B0));
      pragma Assert ((Ca * A0) * (Cb * B0) = (Ca * Cb) * (A0 * B0));
   end Lemma_Product_Low;

   procedure Lemma_Product_Upper (A, B, Am, Bm : Real)
   with
     Ghost,
     Pre  => A >= 0.0 and then A <= Am and then B >= 0.0 and then B <= Bm,
     Post => A * B >= 0.0 and then A * B <= Am * Bm;
   --  Upper bounds of two nonnegative factors multiply.

   procedure Lemma_Product_Upper (A, B, Am, Bm : Real) is
   begin
      Lemma_Nonnegative_Product (A, B);
      Lemma_Scale_Order (A, Am, B);
      Lemma_Scale_Order (B, Bm, Am);
   end Lemma_Product_Upper;

   ---------------------------------------------------------------------------
   --  What the nodal bounds are compared with
   ---------------------------------------------------------------------------

   function Exact_Scale (Q, Fe, D, Ia : Real) return Real
   is ((Q * (1.0 + Fe)) * (D + Ia))
   with Ghost;
   --  A·B: what Scale would be without rounding.

   function Exact_Bracket (I, C, Slip, Size, Ia, Cached_Size, Slope : Real) return Real
   is (abs (I - C) + Slip + Small_Allowance_Real * Size + abs I * (Ia + Small_Allowance_Real)
       + Term_Allowance_Real * Cached_Size
       + Small_Allowance_Real * Slope
       + Seven_Epsilon * (abs I + abs C))
   with Ghost;
   --  X: what the bracket of Nodal_G would be without rounding, less the Epsilon·(|I| + |C|) which pays for the
   --  rounding of I - C.

   ---------------------------------------------------------------------------
   --  The factor error
   ---------------------------------------------------------------------------

   procedure Lemma_Amplification_Is_Limited (Amplification : Dimensionless)
   with
     Ghost,
     Pre  =>
       Amplification >= 16.0
       and then Amplification <= Retained_Side.Largest_Amplification_Value
       and then Factor_Allowance * Amplification <= Largest_Factor_Error,
     Post => Amplification <= Largest_Amplification;
   --  Multiplying by a power of two is exact, so an accepted factor error means an accepted amplification.

   procedure Lemma_Amplification_Is_Limited (Amplification : Dimensionless) is null;

   procedure Lemma_Factor_Error (Amplification : Dimensionless)
   with
     Ghost,
     Pre  => Amplification >= 16.0 and then Amplification <= Retained_Side.Largest_Amplification_Value,
     Post =>
       Factor_Allowance * Amplification in 0.0 .. Factor_Error_Limit
       and then R (Factor_Allowance * Amplification) >= Factor_Slack * R (Amplification)
       and then
         (if Factor_Allowance * Amplification <= Largest_Factor_Error then Amplification <= Largest_Amplification);
   --  Factor_Error is at least the relative error of Q̂ which Retained_Side states, four times over in fact.

   procedure Lemma_Factor_Error (Amplification : Dimensionless) is
   begin
      Lemma_Constants;
      Lemma_Limits;
      pragma Assert (R (Amplification) >= 16.0 and then R (Amplification) <= 1.001E62);
      Lemma_Mul_Bounds_Right (Factor_Allowance, Amplification);
      pragma
        Assert
          (R (Factor_Allowance * Amplification)
           >= (1.0 - Epsilon) * (Factor_Allowance_Real * R (Amplification)) - Eta);
      pragma Assert (R (Factor_Allowance * Amplification) <= 1.0E49);
      Lemma_Float_Bound (Factor_Allowance * Amplification, Factor_Error_Limit);
      if Factor_Allowance * Amplification <= Largest_Factor_Error then
         Lemma_Amplification_Is_Limited (Amplification);
      end if;
   end Lemma_Factor_Error;

   ---------------------------------------------------------------------------
   --  Nodal_E
   ---------------------------------------------------------------------------

   procedure Lemma_E_Chain (Gap, Sum, Qfe, Ia, A1, T2, S1, S2, T3, T4, S3, M, E : Real)
   with
     Ghost,
     Pre  =>
       Gap >= 0.0
       and then Gap <= Sum
       and then Qfe >= 0.0
       and then Ia >= 0.0
       and then A1 >= Gap - Epsilon * Sum - Eta
       and then T2 >= (1.0 - Epsilon) * Qfe - Eta
       and then S1 >= (1.0 - Epsilon) * (A1 + T2) - Eta
       and then S2 >= (1.0 - Epsilon) * (S1 + Ia) - Eta
       and then T3 >= (1.0 - Epsilon) * Sum - Eta
       and then T4 >= (1.0 - Epsilon) * (Small_Allowance_Real * T3) - Eta
       and then S3 >= (1.0 - Epsilon) * (S2 + T4) - Eta
       and then M >= (1.0 - Epsilon) * (S3 * Rounding_Factor_Real) - Eta
       and then E >= (1.0 - Epsilon) * (M + Padding) - Eta,
     Post => E >= E_Factor * (Gap + Qfe + Ia) + Half_Padding;
   --  The nine rounded operations of Nodal_E, each by its lower bound: Gap = |Q̂ - D̂| and Sum = Q̂ + D̂.

   procedure Lemma_E_Chain (Gap, Sum, Qfe, Ia, A1, T2, S1, S2, T3, T4, S3, M, E : Real) is
   begin
      pragma Assert (S1 >= (1.0 - 2.0 * Epsilon) * (Gap + Qfe) - Epsilon * Sum - 3.0 * Eta);
      pragma Assert (S2 >= (1.0 - 3.0 * Epsilon) * (Gap + Qfe + Ia) - Epsilon * Sum - 4.0 * Eta);
      pragma Assert (T4 >= 7.5 * Epsilon * Sum - 2.0 * Eta);
      pragma Assert (S3 >= (1.0 - 4.0 * Epsilon) * (Gap + Qfe + Ia) - 7.0 * Eta);
      pragma Assert (M >= (1.0 - 5.0 * Epsilon) * (Rounding_Factor_Real * (Gap + Qfe + Ia)) - 9.0 * Eta);
   end Lemma_E_Chain;

   procedure Lemma_Nodal_E (Q, D, FE, IA : Dimensionless)
   with
     Ghost,
     Pre  =>
       Q > 0.0
       and then Q <= Largest_Cached_Denominator
       and then D >= 1.0
       and then D <= Largest_Ideal_Denominator
       and then FE >= 0.0
       and then FE <= Largest_Factor_Error
       and then IA >= 0.0
       and then IA <= Ideal_Side.Largest_Allowance,
     Post =>
       Q - D in -E_Limit .. E_Limit
       and then Q * FE in 0.0 .. E_Limit
       and then abs (Q - D) + Q * FE in 0.0 .. E_Limit
       and then abs (Q - D) + Q * FE + IA in 0.0 .. E_Limit
       and then Q + D in 0.0 .. E_Limit
       and then Small_Allowance * (Q + D) in 0.0 .. E_Limit
       and then abs (Q - D) + Q * FE + IA + Small_Allowance * (Q + D) in 0.0 .. E_Limit
       and then (abs (Q - D) + Q * FE + IA + Small_Allowance * (Q + D)) * Rounding_Factor in 0.0 .. E_Limit
       and then
         (abs (Q - D) + Q * FE + IA + Small_Allowance * (Q + D)) * Rounding_Factor + Underflow_Padding
         in 0.0 .. E_Limit
       and then
         R ((abs (Q - D) + Q * FE + IA + Small_Allowance * (Q + D)) * Rounding_Factor + Underflow_Padding)
         >= E_Factor * (abs (R (Q) - R (D)) + R (Q) * R (FE) + R (IA)) + Half_Padding
       and then
         R ((abs (Q - D) + Q * FE + IA + Small_Allowance * (Q + D)) * Rounding_Factor + Underflow_Padding)
         >= Half_Padding
       and then
         R ((abs (Q - D) + Q * FE + IA + Small_Allowance * (Q + D)) * Rounding_Factor + Underflow_Padding) >= R (IA);
   --  Nodal_E as it is computed against what it would be without rounding. It is positive, and at least the
   --  ideal allowance which it includes.

   procedure Lemma_Nodal_E (Q, D, FE, IA : Dimensionless) is
      T1, T2, S1, S2, T3, T4, S3, M, E : Dimensionless;
      Gap                              : constant Real := abs (R (Q) - R (D));
      Sum                              : constant Real := R (Q) + R (D);
      Qfe                              : constant Real := R (Q) * R (FE);
   begin
      Lemma_Constants;
      Lemma_Limits;
      pragma Assert (R (Q) >= 0.0 and then R (Q) <= 1.001E30);
      pragma Assert (R (D) >= 0.0 and then R (D) <= 1.001E30);
      pragma Assert (R (FE) >= 0.0 and then R (FE) <= 0.001);
      pragma Assert (R (IA) >= 0.0 and then R (IA) <= 1.001E100);
      pragma Assert (Gap <= Sum);
      Lemma_Product_Upper (R (Q), R (FE), 1.001E30, 0.001);

      --  |Q - D|
      Lemma_Sub_Bounds (Q, D);
      T1 := Q - D;
      Lemma_Abs_Is_Exact (T1);
      pragma Assert (R (abs T1) >= Gap - Epsilon * Sum - Eta);
      pragma Assert (R (abs T1) <= 2.1E30);
      Lemma_Float_Bound (abs T1, E_Limit);
      pragma Assert (T1 in -E_Limit .. E_Limit);

      --  Q * FE
      Lemma_Mul_Bounds_Right (Q, FE);
      T2 := Q * FE;
      pragma Assert (R (T2) >= (1.0 - Epsilon) * Qfe - Eta);
      pragma Assert (R (T2) <= 1.1E27);
      Lemma_Add_Bounds (abs T1, T2);
      S1 := abs T1 + T2;
      pragma Assert (R (S1) <= 2.2E30);

      --  The ideal allowance
      Lemma_Add_Bounds (S1, IA);
      S2 := S1 + IA;
      pragma Assert (R (S2) <= 1.01E100);

      --  8u * (Q + D)
      Lemma_Add_Bounds (Q, D);
      T3 := Q + D;
      pragma Assert (R (T3) <= 2.1E30);
      Lemma_Mul_Bounds_Right (Small_Allowance, T3);
      T4 := Small_Allowance * T3;
      pragma Assert (R (T4) >= (1.0 - Epsilon) * (Small_Allowance_Real * R (T3)) - Eta);
      pragma Assert (R (T4) <= 1.0E20);
      Lemma_Add_Bounds (S2, T4);
      S3 := S2 + T4;
      pragma Assert (R (S3) <= 1.02E100);

      --  The rounding factor and the padding
      Lemma_Mul_Bounds_Left (S3, Rounding_Factor);
      M := S3 * Rounding_Factor;
      pragma Assert (R (M) >= (1.0 - Epsilon) * (R (S3) * Rounding_Factor_Real) - Eta);
      pragma Assert (R (M) <= 1.03E100);
      Lemma_Add_Bounds (M, Underflow_Padding);
      E := M + Underflow_Padding;
      pragma Assert (R (E) >= (1.0 - Epsilon) * (R (M) + Padding) - Eta);
      pragma Assert (R (E) <= 1.04E100);

      --  None exceeds the limit.
      Lemma_Float_Bound (T2, E_Limit);
      Lemma_Float_Bound (S1, E_Limit);
      Lemma_Float_Bound (S2, E_Limit);
      Lemma_Float_Bound (T3, E_Limit);
      Lemma_Float_Bound (T4, E_Limit);
      Lemma_Float_Bound (S3, E_Limit);
      Lemma_Float_Bound (M, E_Limit);
      Lemma_Float_Bound (E, E_Limit);

      Lemma_E_Chain (Gap, Sum, Qfe, R (IA), R (abs T1), R (T2), R (S1), R (S2), R (T3), R (T4), R (S3), R (M), R (E));
      pragma Assert (R (E) >= E_Factor * (Gap + Qfe + R (IA)) + Half_Padding);
   end Lemma_Nodal_E;

   procedure Lemma_E_Covers (Q, D, Qh, Dh, Amp, Fe, Ia, E : Real)
   with
     Ghost,
     Pre  =>
       Qh >= 0.0
       and then Amp >= 0.0
       and then Ia >= 0.0
       and then Fe >= Factor_Slack * Amp
       and then abs (Qh - Q) <= Factor_Slack * Amp * Qh
       and then abs (Dh - D) <= Ia * (1.0 + Slack) + Denominator_Dust
       and then E >= E_Factor * (abs (Qh - Dh) + Qh * Fe + Ia) + Half_Padding,
     Post => abs (Q - D) <= E;
   --  |Q - D| ≤ |Q̂ - D̂| + |Q̂ - Q| + |D̂ - D|, and E_Factor covers the slack, Half_Padding the dust.

   procedure Lemma_E_Covers (Q, D, Qh, Dh, Amp, Fe, Ia, E : Real) is
   begin
      Lemma_Scale_Order (Factor_Slack * Amp, Fe, Qh);
      pragma Assert (abs (Qh - Q) <= Qh * Fe);
      pragma Assert (abs (Q - D) <= abs (Qh - Dh) + abs (Qh - Q) + abs (Dh - D));
      pragma Assert (Ia * (1.0 + Slack) + Denominator_Dust <= E_Factor * Ia + Half_Padding);
   end Lemma_E_Covers;

   function Stages_Are_Accurate
     (Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Ideal             : Ideal_Side.Ideal_Node;
      Retained          : Retained_Side.Retained_Node;
      X                 : Real) return Boolean
   with
     Ghost,
     Pre  => Has_Layout (Cache),
     Post =>
       Stages_Are_Accurate'Result
       = (Ideal_Side.Is_Node_Accurate (Coefficients, Prepared.Delta_Y, Ideal, X)
          and then Ideal_Side.Is_Node_Tight (Coefficients, Prepared.Delta_Y, Ideal, X)
          and then Retained_Side.Is_Node_Accurate (Warp, Cache, Denominator_Scale, Prepared, Retained, X));
   --  What the two stages state about their results at an accurate sample. It is not an expression function:
   --  Bounds_At_Node carries it as one fact, and only the two lemmas which use it look inside.

   function Stages_Are_Accurate
     (Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Ideal             : Ideal_Side.Ideal_Node;
      Retained          : Retained_Side.Retained_Node;
      X                 : Real) return Boolean is
   begin
      return
        Ideal_Side.Is_Node_Accurate (Coefficients, Prepared.Delta_Y, Ideal, X)
        and then Ideal_Side.Is_Node_Tight (Coefficients, Prepared.Delta_Y, Ideal, X)
        and then Retained_Side.Is_Node_Accurate (Warp, Cache, Denominator_Scale, Prepared, Retained, X);
   end Stages_Are_Accurate;

   procedure Lemma_Denominator_Error_Is_Covered
     (Accurate          : Boolean;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Ideal             : Ideal_Side.Ideal_Node;
      Retained          : Retained_Side.Retained_Node;
      Factor_Error      : Dimensionless;
      Nodal_E           : Dimensionless;
      X                 : Real)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Retained.Cached_Q >= 0.0
       and then Retained.Amplification >= 0.0
       and then Ideal.Ideal_Allowance >= 0.0
       and then R (Factor_Error) >= Factor_Slack * R (Retained.Amplification)
       and then
         R (Nodal_E)
         >= E_Factor
            * (abs (R (Retained.Cached_Q) - R (Ideal.Ideal_Q))
               + R (Retained.Cached_Q) * R (Factor_Error)
               + R (Ideal.Ideal_Allowance))
            + Half_Padding
       and then
         (if Accurate
          then Stages_Are_Accurate (Coefficients, Warp, Cache, Denominator_Scale, Prepared, Ideal, Retained, X)),
     Post =>
       (if Accurate then abs Model.Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, X) <= R (Nodal_E));
   --  The two stages are accurate at an accurate sample, and then Nodal_E is at least |e| there.

   procedure Lemma_Denominator_Error_Is_Covered
     (Accurate          : Boolean;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Ideal             : Ideal_Side.Ideal_Node;
      Retained          : Retained_Side.Retained_Node;
      Factor_Error      : Dimensionless;
      Nodal_E           : Dimensionless;
      X                 : Real) is
   begin
      if Accurate then
         pragma Assert (Ideal_Side.Is_Node_Accurate (Coefficients, Prepared.Delta_Y, Ideal, X));
         pragma Assert (Retained_Side.Is_Node_Accurate (Warp, Cache, Denominator_Scale, Prepared, Retained, X));
         pragma
           Assert
             (abs (R (Retained.Cached_Q) - Model.Cached_Denominator (Cache, Denominator_Scale, Warp, X))
              <= Factor_Slack * R (Retained.Amplification) * R (Retained.Cached_Q));
         pragma
           Assert
             (abs (R (Ideal.Ideal_Q) - Denominator (Coefficients, X))
              <= R (Ideal.Ideal_Allowance) * (1.0 + Slack) + Denominator_Dust);
         Lemma_E_Covers
           (Q   => Model.Cached_Denominator (Cache, Denominator_Scale, Warp, X),
            D   => Denominator (Coefficients, X),
            Qh  => R (Retained.Cached_Q),
            Dh  => R (Ideal.Ideal_Q),
            Amp => R (Retained.Amplification),
            Fe  => R (Factor_Error),
            Ia  => R (Ideal.Ideal_Allowance),
            E   => R (Nodal_E));
      end if;
   end Lemma_Denominator_Error_Is_Covered;

   ---------------------------------------------------------------------------
   --  Scale
   ---------------------------------------------------------------------------

   procedure Lemma_Scale (Q, FE, D, IA : Dimensionless)
   with
     Ghost,
     Pre  =>
       Q >= Smallest_Partial_Product
       and then Q <= Largest_Cached_Denominator
       and then FE >= 0.0
       and then FE <= Largest_Factor_Error
       and then D >= 1.0
       and then D <= Largest_Ideal_Denominator
       and then IA >= 0.0
       and then IA <= 1.0,
     Post =>
       1.0 + FE in 0.0 .. 2.0
       and then Q * (1.0 + FE) in 0.0 .. 1.1E30
       and then D + IA in 0.0 .. 1.1E30
       and then (Q * (1.0 + FE)) * (D + IA) in 0.0 .. Scale_Limit
       and then Exact_Scale (R (Q), R (FE), R (D), R (IA)) >= 0.0
       and then Exact_Scale (R (Q), R (FE), R (D), R (IA)) <= Scale_Bound
       and then R ((Q * (1.0 + FE)) * (D + IA)) >= Scale_Factor * Exact_Scale (R (Q), R (FE), R (D), R (IA));
   --  Scale as it is computed against what it would be without rounding. Nothing here is small enough to
   --  underflow, so the four operations lose two Epsilon each and no more.

   procedure Lemma_Scale (Q, FE, D, IA : Dimensionless) is
      F1, P1, F2, S : Dimensionless;
      Ef            : constant Real := 1.0 + R (FE);
      Qf            : constant Real := R (Q) * Ef;
      Dx            : constant Real := R (D) + R (IA);
   begin
      Lemma_Constants;
      Lemma_Limits;
      pragma Assert (Q >= 0.0);
      pragma Assert (R (Q) >= 1.0E-101 and then R (Q) <= 1.001E30);
      pragma Assert (R (FE) >= 0.0 and then R (FE) <= 0.001);
      pragma Assert (R (D) >= 1.0 and then R (D) <= 1.001E30);
      pragma Assert (R (IA) >= 0.0 and then R (IA) <= 1.0);

      --  The exact value and its bound.
      Lemma_Product_Upper (R (Q), Ef, 1.001E30, 1.001);
      Lemma_Product_Upper (Qf, Dx, 1.001E30 * 1.001, 1.002E30);
      pragma Assert (Exact_Scale (R (Q), R (FE), R (D), R (IA)) = Qf * Dx);
      pragma Assert (Qf * Dx <= Scale_Bound);

      --  1 + FE
      Lemma_Add_Bounds (1.0, FE);
      F1 := 1.0 + FE;
      Lemma_Relative_Low (R (F1), Ef);
      pragma Assert (R (F1) >= 0.5 and then R (F1) <= 1.01);

      --  Q * (1 + FE)
      Lemma_Mul_Bounds_Right (Q, F1);
      P1 := Q * F1;
      Lemma_Product_Low (R (Q), R (F1), R (Q), Ef, 1.0, 1.0 - 2.0 * Epsilon);
      pragma Assert (R (Q) * R (F1) >= (1.0 - 2.0 * Epsilon) * Qf);
      Lemma_Scale_Order (0.5, R (F1), R (Q));
      pragma Assert (R (Q) * R (F1) >= Normal_Floor);
      Lemma_Relative_Low (R (P1), R (Q) * R (F1));
      pragma Assert (R (P1) >= (1.0 - 4.0 * Epsilon) * Qf);
      Lemma_Scale_Order (1.0, Ef, R (Q));
      pragma Assert (Qf >= 1.0E-101);
      Lemma_Product_Upper (R (Q), R (F1), 1.001E30, 1.01);
      pragma Assert (R (P1) <= 1.02E30);

      --  D + IA
      Lemma_Add_Bounds (D, IA);
      F2 := D + IA;
      Lemma_Relative_Low (R (F2), Dx);
      pragma Assert (R (F2) >= 0.5 and then R (F2) <= 1.01E30);

      --  The product
      Lemma_Mul_Bounds_Right (P1, F2);
      S := P1 * F2;
      Lemma_Product_Low (R (P1), R (F2), Qf, Dx, 1.0 - 4.0 * Epsilon, 1.0 - 2.0 * Epsilon);
      pragma Assert (R (P1) * R (F2) >= ((1.0 - 4.0 * Epsilon) * (1.0 - 2.0 * Epsilon)) * (Qf * Dx));
      Lemma_Scale_Order (1.0, Dx, Qf);
      pragma Assert (Qf * Dx >= 1.0E-101);
      pragma Assert (R (P1) * R (F2) >= Normal_Floor);
      Lemma_Relative_Low (R (S), R (P1) * R (F2));
      pragma Assert (R (S) >= Scale_Factor * (Qf * Dx));
      Lemma_Product_Upper (R (P1), R (F2), 1.02E30, 1.01E30);
      pragma Assert (R (S) <= 1.04E60);

      --  None exceeds its limit.
      Lemma_Float_Bound (F1, 2.0);
      Lemma_Float_Bound (P1, 1.1E30);
      Lemma_Float_Bound (F2, 1.1E30);
      Lemma_Float_Bound (S, Scale_Limit);
   end Lemma_Scale;

   ---------------------------------------------------------------------------
   --  The bracket of Nodal_G
   ---------------------------------------------------------------------------

   procedure Lemma_Cached (Retained, Constant_Tangent, Size : Dimensionless)
   with
     Ghost,
     Pre  =>
       abs Retained <= Retained_Side.Largest_Retained_Value
       and then abs Constant_Tangent <= Magnitude_Limit
       and then Size >= 0.0
       and then Size <= Retained_Side.Largest_Retained_Value,
     Post =>
       Retained + Constant_Tangent in -Cached_Limit .. Cached_Limit
       and then Size + abs Constant_Tangent in 0.0 .. Cached_Limit
       and then
         abs (R (Retained + Constant_Tangent) - (R (Retained) + R (Constant_Tangent)))
         <= Epsilon * abs (R (Retained) + R (Constant_Tangent)) + Eta
       and then R (Size + abs Constant_Tangent) >= (1.0 - Epsilon) * R (Size) - Eta;
   --  The retained tangent with its constant, and the sum of the magnitudes of its terms with that of the
   --  constant, as they round.

   procedure Lemma_Cached (Retained, Constant_Tangent, Size : Dimensionless) is
      Cached, Cached_Size : Dimensionless;
   begin
      Lemma_Limits;
      Lemma_Abs_Is_Exact (Retained);
      Lemma_Abs_Is_Exact (Constant_Tangent);
      pragma Assert (abs R (Retained) <= 1.001E185);
      pragma Assert (abs R (Constant_Tangent) <= 1.001E60);
      pragma Assert (R (Size) >= 0.0 and then R (Size) <= 1.001E185);

      Lemma_Signed_Add_Bounds (Retained, Constant_Tangent);
      Cached := Retained + Constant_Tangent;
      Lemma_Abs_Is_Exact (Cached);
      pragma Assert (R (abs Cached) <= 1.01E185);
      Lemma_Float_Bound (abs Cached, Cached_Limit);
      pragma Assert (Cached in -Cached_Limit .. Cached_Limit);

      Lemma_Add_Bounds (Size, abs Constant_Tangent);
      Cached_Size := Size + abs Constant_Tangent;
      pragma Assert (R (Cached_Size) <= 1.01E185);
      Lemma_Float_Bound (Cached_Size, Cached_Limit);
      pragma Assert (R (Cached_Size) >= (1.0 - Epsilon) * R (Size) - Eta);
   end Lemma_Cached;

   procedure Lemma_Bracket_Sum
     (Gap, Mag, Slip, Sn, P4, Tc, Ts, A1, B3, B4, B5, B6, T7, B7, S1, S2, S3, S4, S5, S6 : Real)
   with
     Ghost,
     Pre  =>
       Gap >= 0.0
       and then Gap <= Mag
       and then Slip >= 0.0
       and then Sn >= 0.0
       and then P4 >= 0.0
       and then Tc >= 0.0
       and then Ts >= 0.0
       and then A1 >= Gap - Epsilon * Mag - Eta
       and then B3 >= (1.0 - Epsilon) * Sn - Eta
       and then B4 >= (1.0 - 3.0 * Epsilon) * P4 - Eta
       and then B5 >= (1.0 - Epsilon) * Tc - Eta
       and then B6 >= (1.0 - Epsilon) * Ts - Eta
       and then T7 >= (1.0 - Epsilon) * Mag - Eta
       and then B7 >= (1.0 - Epsilon) * (Small_Allowance_Real * T7) - Eta
       and then S1 >= (1.0 - Epsilon) * (A1 + Slip) - Eta
       and then S2 >= (1.0 - Epsilon) * (S1 + B3) - Eta
       and then S3 >= (1.0 - Epsilon) * (S2 + B4) - Eta
       and then S4 >= (1.0 - Epsilon) * (S3 + B5) - Eta
       and then S5 >= (1.0 - Epsilon) * (S4 + B6) - Eta
       and then S6 >= (1.0 - Epsilon) * (S5 + B7) - Eta,
     Post => S6 >= Bracket_Factor * (Gap + Slip + Sn + P4 + Tc + Ts + Seven_Epsilon * Mag) - Bracket_Dust;
   --  The rounded operations of the bracket, each by its lower bound: Gap = |Î - Ĉ| and Mag = |Î| + |Ĉ|, and
   --  Sn, P4, Tc and Ts are the exact values of the four products.

   procedure Lemma_Bracket_Sum
     (Gap, Mag, Slip, Sn, P4, Tc, Ts, A1, B3, B4, B5, B6, T7, B7, S1, S2, S3, S4, S5, S6 : Real) is
   begin
      pragma Assert (S1 >= (1.0 - Epsilon) * (Gap + Slip) - Epsilon * Mag - 2.0 * Eta);
      pragma Assert (S2 >= (1.0 - 2.0 * Epsilon) * (Gap + Slip + Sn) - Epsilon * Mag - 4.0 * Eta);
      pragma Assert (S3 >= (1.0 - 5.0 * Epsilon) * (Gap + Slip + Sn + P4) - Epsilon * Mag - 6.0 * Eta);
      pragma Assert (S4 >= (1.0 - 6.0 * Epsilon) * (Gap + Slip + Sn + P4 + Tc) - Epsilon * Mag - 8.0 * Eta);
      pragma Assert (S5 >= (1.0 - 7.0 * Epsilon) * (Gap + Slip + Sn + P4 + Tc + Ts) - Epsilon * Mag - 10.0 * Eta);
      pragma Assert (B7 >= 7.5 * Epsilon * Mag - 2.0 * Eta);
   end Lemma_Bracket_Sum;

   procedure Lemma_Bracket_Chain
     (I, C, Slip, Size, Ia, Csz, Sl, A1, B3, B4, B5, B6, T7, B7, S1, S2, S3, S4, S5, S6 : Real)
   with
     Ghost,
     Pre  =>
       Slip >= 0.0
       and then Size >= 0.0
       and then Ia >= 0.0
       and then Csz >= 0.0
       and then Sl >= 0.0
       and then A1 >= abs (I - C) - Epsilon * (abs I + abs C) - Eta
       and then B3 >= (1.0 - Epsilon) * (Small_Allowance_Real * Size) - Eta
       and then B4 >= (1.0 - 3.0 * Epsilon) * (abs I * (Ia + Small_Allowance_Real)) - Eta
       and then B5 >= (1.0 - Epsilon) * (Term_Allowance_Real * Csz) - Eta
       and then B6 >= (1.0 - Epsilon) * (Small_Allowance_Real * Sl) - Eta
       and then T7 >= (1.0 - Epsilon) * (abs I + abs C) - Eta
       and then B7 >= (1.0 - Epsilon) * (Small_Allowance_Real * T7) - Eta
       and then S1 >= (1.0 - Epsilon) * (A1 + Slip) - Eta
       and then S2 >= (1.0 - Epsilon) * (S1 + B3) - Eta
       and then S3 >= (1.0 - Epsilon) * (S2 + B4) - Eta
       and then S4 >= (1.0 - Epsilon) * (S3 + B5) - Eta
       and then S5 >= (1.0 - Epsilon) * (S4 + B6) - Eta
       and then S6 >= (1.0 - Epsilon) * (S5 + B7) - Eta,
     Post =>
       Exact_Bracket (I, C, Slip, Size, Ia, Csz, Sl) >= 0.0
       and then S6 >= Bracket_Factor * Exact_Bracket (I, C, Slip, Size, Ia, Csz, Sl) - Bracket_Dust;
   --  The same over the real values of the operands, against the exact bracket.

   procedure Lemma_Bracket_Chain
     (I, C, Slip, Size, Ia, Csz, Sl, A1, B3, B4, B5, B6, T7, B7, S1, S2, S3, S4, S5, S6 : Real)
   is
      Gap : constant Real := abs (I - C);
      Mag : constant Real := abs I + abs C;
      P4  : constant Real := abs I * (Ia + Small_Allowance_Real);
   begin
      Lemma_Nonnegative_Product (abs I, Ia + Small_Allowance_Real);
      pragma Assert (Gap <= Mag);
      pragma
        Assert
          (Exact_Bracket (I, C, Slip, Size, Ia, Csz, Sl)
           = Gap + Slip + Small_Allowance_Real * Size + P4 + Term_Allowance_Real * Csz + Small_Allowance_Real * Sl
             + Seven_Epsilon * Mag);
      Lemma_Bracket_Sum
        (Gap  => Gap,
         Mag  => Mag,
         Slip => Slip,
         Sn   => Small_Allowance_Real * Size,
         P4   => P4,
         Tc   => Term_Allowance_Real * Csz,
         Ts   => Small_Allowance_Real * Sl,
         A1   => A1,
         B3   => B3,
         B4   => B4,
         B5   => B5,
         B6   => B6,
         T7   => T7,
         B7   => B7,
         S1   => S1,
         S2   => S2,
         S3   => S3,
         S4   => S4,
         S5   => S5,
         S6   => S6);
   end Lemma_Bracket_Chain;

   procedure Lemma_Bracket (Ideal, Cached, Slip, Size, Allowance, Cached_Size, Slope : Dimensionless)
   with
     Ghost,
     Pre  =>
       abs Ideal <= Ideal_Side.Largest_Axis_Value
       and then abs Cached <= Cached_Limit
       and then Slip >= 0.0
       and then Slip <= Ideal_Side.Largest_Axis_Value
       and then Size >= 0.0
       and then Size <= Ideal_Side.Largest_Axis_Value
       and then Allowance >= 0.0
       and then Allowance <= 1.0
       and then Cached_Size >= 0.0
       and then Cached_Size <= Cached_Limit
       and then Slope >= 0.0
       and then Slope <= Poles.Largest_Slope,
     Post =>
       Ideal - Cached in -Bracket_Limit .. Bracket_Limit
       and then abs (Ideal - Cached) + Slip in 0.0 .. Bracket_Limit
       and then Small_Allowance * Size in 0.0 .. Bracket_Limit
       and then abs (Ideal - Cached) + Slip + Small_Allowance * Size in 0.0 .. Bracket_Limit
       and then Allowance + Small_Allowance in 0.0 .. 2.0
       and then abs Ideal * (Allowance + Small_Allowance) in 0.0 .. Bracket_Limit
       and then
         abs (Ideal - Cached) + Slip + Small_Allowance * Size + abs Ideal * (Allowance + Small_Allowance)
         in 0.0 .. Bracket_Limit
       and then Term_Allowance * Cached_Size in 0.0 .. Bracket_Limit
       and then
         abs (Ideal - Cached) + Slip + Small_Allowance * Size + abs Ideal * (Allowance + Small_Allowance)
         + Term_Allowance * Cached_Size
         in 0.0 .. Bracket_Limit
       and then Small_Allowance * Slope in 0.0 .. Bracket_Limit
       and then
         abs (Ideal - Cached) + Slip + Small_Allowance * Size + abs Ideal * (Allowance + Small_Allowance)
         + Term_Allowance * Cached_Size
         + Small_Allowance * Slope
         in 0.0 .. Bracket_Limit
       and then abs Ideal + abs Cached in 0.0 .. Bracket_Limit
       and then Small_Allowance * (abs Ideal + abs Cached) in 0.0 .. Bracket_Limit
       and then
         abs (Ideal - Cached) + Slip + Small_Allowance * Size + abs Ideal * (Allowance + Small_Allowance)
         + Term_Allowance * Cached_Size
         + Small_Allowance * Slope
         + Small_Allowance * (abs Ideal + abs Cached)
         in 0.0 .. Bracket_Limit
       and then
         Exact_Bracket (R (Ideal), R (Cached), R (Slip), R (Size), R (Allowance), R (Cached_Size), R (Slope)) >= 0.0
       and then
         R
           (abs (Ideal - Cached) + Slip + Small_Allowance * Size + abs Ideal * (Allowance + Small_Allowance)
            + Term_Allowance * Cached_Size
            + Small_Allowance * Slope
            + Small_Allowance * (abs Ideal + abs Cached))
         >= Bracket_Factor
            * Exact_Bracket (R (Ideal), R (Cached), R (Slip), R (Size), R (Allowance), R (Cached_Size), R (Slope))
            - Bracket_Dust;
   --  The bracket of Nodal_G as it is computed, with every intermediate value within its limit, against what it
   --  would be without rounding.

   procedure Lemma_Bracket (Ideal, Cached, Slip, Size, Allowance, Cached_Size, Slope : Dimensionless) is
      T1, S1, B3, S2, F4, B4, S3, B5, S4, B6, S5, T7, B7, S6 : Dimensionless;
      Gap                                                    : constant Real := abs (R (Ideal) - R (Cached));
      Mag                                                    : constant Real := abs R (Ideal) + abs R (Cached);
      Ax                                                     : constant Real := R (Allowance) + Small_Allowance_Real;
      P4                                                     : constant Real := abs R (Ideal) * Ax;
   begin
      Lemma_Constants;
      Lemma_Limits;
      Lemma_Abs_Is_Exact (Ideal);
      Lemma_Abs_Is_Exact (Cached);
      pragma Assert (abs R (Ideal) <= 1.001E170);
      pragma Assert (abs R (Cached) <= 1.101E185);
      pragma Assert (R (Slip) >= 0.0 and then R (Slip) <= 1.001E170);
      pragma Assert (R (Size) >= 0.0 and then R (Size) <= 1.001E170);
      pragma Assert (R (Allowance) >= 0.0 and then R (Allowance) <= 1.0);
      pragma Assert (R (Cached_Size) >= 0.0 and then R (Cached_Size) <= 1.101E185);
      pragma Assert (R (Slope) >= 0.0 and then R (Slope) <= 1.001E125);
      pragma Assert (Gap <= Mag);
      Lemma_Product_Upper (abs R (Ideal), Ax, 1.001E170, 1.001);

      --  |Ideal - Cached| + Slip
      Lemma_Sub_Bounds (Ideal, Cached);
      T1 := Ideal - Cached;
      Lemma_Abs_Is_Exact (T1);
      pragma Assert (R (abs T1) >= Gap - Epsilon * Mag - Eta);
      pragma Assert (R (abs T1) <= 1.103E185);
      Lemma_Float_Bound (abs T1, Bracket_Limit);
      pragma Assert (T1 in -Bracket_Limit .. Bracket_Limit);
      Lemma_Add_Bounds (abs T1, Slip);
      S1 := abs T1 + Slip;
      pragma Assert (R (S1) <= 1.104E185);

      --  8u * Size
      Lemma_Mul_Bounds_Right (Small_Allowance, Size);
      B3 := Small_Allowance * Size;
      pragma Assert (R (B3) >= (1.0 - Epsilon) * (Small_Allowance_Real * R (Size)) - Eta);
      pragma Assert (R (B3) <= 1.0E156);
      Lemma_Add_Bounds (S1, B3);
      S2 := S1 + B3;
      pragma Assert (R (S2) <= 1.105E185);

      --  |Ideal| * (Allowance + 8u)
      Lemma_Add_Bounds (Allowance, Small_Allowance);
      F4 := Allowance + Small_Allowance;
      Lemma_Relative_Low (R (F4), Ax);
      pragma Assert (R (F4) <= 1.01);
      Lemma_Mul_Bounds_Left (abs Ideal, F4);
      B4 := abs Ideal * F4;
      Lemma_Product_Low (abs R (Ideal), R (F4), abs R (Ideal), Ax, 1.0, 1.0 - 2.0 * Epsilon);
      pragma Assert (R (abs Ideal) * R (F4) >= (1.0 - 2.0 * Epsilon) * P4);
      pragma Assert (R (B4) >= (1.0 - 3.0 * Epsilon) * P4 - Eta);
      Lemma_Product_Upper (abs R (Ideal), R (F4), 1.001E170, 1.01);
      pragma Assert (R (B4) <= 1.02E170);
      Lemma_Add_Bounds (S2, B4);
      S3 := S2 + B4;
      pragma Assert (R (S3) <= 1.106E185);

      --  64u * Cached_Size
      Lemma_Mul_Bounds_Right (Term_Allowance, Cached_Size);
      B5 := Term_Allowance * Cached_Size;
      pragma Assert (R (B5) >= (1.0 - Epsilon) * (Term_Allowance_Real * R (Cached_Size)) - Eta);
      pragma Assert (R (B5) <= 1.0E171);
      Lemma_Add_Bounds (S3, B5);
      S4 := S3 + B5;
      pragma Assert (R (S4) <= 1.107E185);

      --  8u * Slope
      Lemma_Mul_Bounds_Right (Small_Allowance, Slope);
      B6 := Small_Allowance * Slope;
      pragma Assert (R (B6) >= (1.0 - Epsilon) * (Small_Allowance_Real * R (Slope)) - Eta);
      pragma Assert (R (B6) <= 1.0E111);
      Lemma_Add_Bounds (S4, B6);
      S5 := S4 + B6;
      pragma Assert (R (S5) <= 1.108E185);

      --  8u * (|Ideal| + |Cached|)
      Lemma_Add_Bounds (abs Ideal, abs Cached);
      T7 := abs Ideal + abs Cached;
      pragma Assert (R (T7) >= (1.0 - Epsilon) * Mag - Eta);
      pragma Assert (R (T7) <= 1.103E185);
      Lemma_Mul_Bounds_Right (Small_Allowance, T7);
      B7 := Small_Allowance * T7;
      pragma Assert (R (B7) >= (1.0 - Epsilon) * (Small_Allowance_Real * R (T7)) - Eta);
      pragma Assert (R (B7) <= 1.0E171);
      Lemma_Add_Bounds (S5, B7);
      S6 := S5 + B7;
      pragma Assert (R (S6) <= 1.109E185);

      --  None exceeds its limit.
      Lemma_Float_Bound (S1, Bracket_Limit);
      Lemma_Float_Bound (B3, Bracket_Limit);
      Lemma_Float_Bound (S2, Bracket_Limit);
      Lemma_Float_Bound (F4, 2.0);
      Lemma_Float_Bound (B4, Bracket_Limit);
      Lemma_Float_Bound (S3, Bracket_Limit);
      Lemma_Float_Bound (B5, Bracket_Limit);
      Lemma_Float_Bound (S4, Bracket_Limit);
      Lemma_Float_Bound (B6, Bracket_Limit);
      Lemma_Float_Bound (S5, Bracket_Limit);
      Lemma_Float_Bound (T7, Bracket_Limit);
      Lemma_Float_Bound (B7, Bracket_Limit);
      Lemma_Float_Bound (S6, Bracket_Limit);

      Lemma_Bracket_Chain
        (I    => R (Ideal),
         C    => R (Cached),
         Slip => R (Slip),
         Size => R (Size),
         Ia   => R (Allowance),
         Csz  => R (Cached_Size),
         Sl   => R (Slope),
         A1   => R (abs T1),
         B3   => R (B3),
         B4   => R (B4),
         B5   => R (B5),
         B6   => R (B6),
         T7   => R (T7),
         B7   => R (B7),
         S1   => R (S1),
         S2   => R (S2),
         S3   => R (S3),
         S4   => R (S4),
         S5   => R (S5),
         S6   => R (S6));
   end Lemma_Bracket;

   ---------------------------------------------------------------------------
   --  Nodal_G
   ---------------------------------------------------------------------------

   procedure Lemma_Product_Chain (Sb, W, M, G : Real)
   with
     Ghost,
     Pre  =>
       Sb >= 0.0
       and then W >= (1.0 - Epsilon) * Sb - Eta
       and then M >= (1.0 - Epsilon) * (W * Rounding_Factor_Real) - Eta
       and then G >= (1.0 - Epsilon) * (M + Padding) - Eta,
     Post => G >= Product_Factor * Sb + Padding_Part;
   --  The last three rounded operations of Nodal_G, each by its lower bound.

   procedure Lemma_Product_Chain (Sb, W, M, G : Real) is null;

   procedure Lemma_G_Lower (G, S, B, Sx, X : Real)
   with
     Ghost,
     Pre  =>
       Sx >= 0.0
       and then X >= 0.0
       and then S >= 0.0
       and then S <= Scale_Bound
       and then S >= Scale_Factor * Sx
       and then B >= Bracket_Factor * X - Bracket_Dust
       and then G >= Product_Factor * (S * B) + Padding_Part,
     Post => G >= G_Factor * (Sx * X) + Padding_Left;
   --  The three factors lost to rounding still leave more than G_Factor, and the dust of the bracket, as Scale
   --  amplifies it, is far below the padding.

   procedure Lemma_G_Lower (G, S, B, Sx, X : Real) is
   begin
      Lemma_Scale_Order (Bracket_Factor * X - Bracket_Dust, B, S);
      pragma Assert (S * B >= S * (Bracket_Factor * X - Bracket_Dust));
      pragma Assert (S * (Bracket_Factor * X - Bracket_Dust) = Bracket_Factor * (S * X) - Bracket_Dust * S);
      Lemma_Scale_Order (Scale_Factor * Sx, S, X);
      pragma Assert (S * X >= (Scale_Factor * Sx) * X);
      pragma Assert ((Scale_Factor * Sx) * X = Scale_Factor * (Sx * X));
      Lemma_Nonnegative_Product (Sx, X);
      pragma Assert (S * B >= (Bracket_Factor * Scale_Factor) * (Sx * X) - Bracket_Dust * Scale_Bound);
   end Lemma_G_Lower;

   procedure Lemma_Nodal_G (Scale, Bracket : Dimensionless; Sx, X : Real)
   with
     Ghost,
     Pre  =>
       Scale in 0.0 .. Scale_Limit
       and then Bracket in 0.0 .. Bracket_Limit
       and then Sx >= 0.0
       and then X >= 0.0
       and then R (Scale) >= Scale_Factor * Sx
       and then R (Bracket) >= Bracket_Factor * X - Bracket_Dust,
     Post =>
       Scale * Bracket in 0.0 .. Product_Limit
       and then Scale * Bracket * Rounding_Factor in 0.0 .. Padded_Limit
       and then Scale * Bracket * Rounding_Factor + Underflow_Padding in 0.0 .. Padded_Limit
       and then R (Scale * Bracket * Rounding_Factor + Underflow_Padding) >= G_Factor * (Sx * X) + Padding_Left;
   --  Nodal_G as it is computed from Scale and the bracket, against the exact values which those two
   --  approximate from below.

   procedure Lemma_Nodal_G (Scale, Bracket : Dimensionless; Sx, X : Real) is
      W, M, G : Dimensionless;
   begin
      Lemma_Constants;
      Lemma_Limits;
      pragma Assert (R (Scale) >= 0.0 and then R (Scale) <= 1.101E60);
      pragma Assert (R (Bracket) >= 0.0 and then R (Bracket) <= 1.201E185);
      Lemma_Product_Upper (R (Scale), R (Bracket), 1.101E60, 1.201E185);

      Lemma_Mul_Bounds_Right (Scale, Bracket);
      W := Scale * Bracket;
      pragma Assert (R (W) <= 1.33E245);
      Lemma_Mul_Bounds_Left (W, Rounding_Factor);
      M := W * Rounding_Factor;
      pragma Assert (R (M) >= (1.0 - Epsilon) * (R (W) * Rounding_Factor_Real) - Eta);
      pragma Assert (R (M) <= 1.34E245);
      Lemma_Add_Bounds (M, Underflow_Padding);
      G := M + Underflow_Padding;
      pragma Assert (R (G) >= (1.0 - Epsilon) * (R (M) + Padding) - Eta);
      pragma Assert (R (G) <= 1.35E245);

      --  None exceeds its limit.
      Lemma_Float_Bound (W, Product_Limit);
      Lemma_Float_Bound (M, Padded_Limit);
      Lemma_Float_Bound (G, Padded_Limit);

      Lemma_Product_Chain (R (Scale) * R (Bracket), R (W), R (M), R (G));
      Lemma_G_Lower (R (G), R (Scale), R (Bracket), Sx, X);
   end Lemma_Nodal_G;

   ---------------------------------------------------------------------------
   --  Nodal_G against g
   ---------------------------------------------------------------------------

   procedure Lemma_Tangent_Gap (T, Rv, I, C, Rk, Slip, Sn, P4, Sz, Csz, Sl : Real)
   with
     Ghost,
     Pre  =>
       Slip >= 0.0
       and then Sn >= 0.0
       and then P4 >= 0.0
       and then Sz >= 0.0
       and then Csz >= 0.0
       and then Sl >= 0.0
       and then abs (I - T) <= (Slip + Sn + P4) * (1.0 + Slack) + Tangent_Dust
       and then abs (Rk - Rv) <= Term_Slack * Sz + Slope_Slack * Sl + Retained_Dust
       and then abs (C - Rk) <= Epsilon * abs Rk + Eta
       and then Csz >= (1.0 - Epsilon) * Sz - Eta,
     Post =>
       abs (T - Rv)
       <= (1.0 + Slack)
          * (abs (I - C) + Slip + Sn + P4 + Term_Allowance_Real * Csz + Small_Allowance_Real * Sl
             + Seven_Epsilon * (abs I + abs C))
          + Gap_Dust;
   --  |T - R| ≤ |Î - Ĉ| + |Î - T| + |Ĉ - (R̂ + c)| + |R̂ + c - R|. Rounding R̂ + c to Ĉ costs at most
   --  2u·|Ĉ| + 2·Eta, and Term_Slack·Size is at most Term_Allowance·Ĉsize + Eta.

   procedure Lemma_Tangent_Gap (T, Rv, I, C, Rk, Slip, Sn, P4, Sz, Csz, Sl : Real) is
   begin
      pragma Assert (abs Rk <= abs C + abs (C - Rk));
      pragma Assert (abs (C - Rk) <= 2.0 * Epsilon * abs C + 2.0 * Eta);
      pragma Assert (Term_Slack * Sz <= Term_Allowance_Real * Csz + Eta);
      pragma Assert (abs (Rk - Rv) <= Term_Allowance_Real * Csz + Small_Allowance_Real * Sl + Retained_Dust + Eta);
      pragma Assert (abs (T - Rv) <= abs (I - T) + abs (I - C) + abs (C - Rk) + abs (Rk - Rv));
   end Lemma_Tangent_Gap;

   procedure Lemma_Numerator_Product (Q, D, P, T, Rv, Qb, Db, Tb : Real)
   with
     Ghost,
     Pre  => D >= 0.0 and then T * D = P and then abs Q <= Qb and then D <= Db and then abs (T - Rv) <= Tb,
     Post => abs (Q * (P - D * Rv)) <= Qb * (Db * Tb);
   --  g = Q·(P - D·R) = Q·D·(T - R), bounded factor by factor.

   procedure Lemma_Numerator_Product (Q, D, P, T, Rv, Qb, Db, Tb : Real) is
      Z : constant Real := P - D * Rv;
   begin
      pragma Assert (D * (T - Rv) = T * D - D * Rv);
      pragma Assert (Z = D * (T - Rv));
      pragma Assert (abs Z = D * abs (T - Rv));
      Lemma_Scale_Order (abs (T - Rv), Tb, D);
      Lemma_Scale_Order (D, Db, Tb);
      pragma Assert (abs Z <= Db * Tb);
      pragma Assert (abs (Q * Z) = abs Q * abs Z);
      Lemma_Scale_Order (abs Q, Qb, abs Z);
      Lemma_Scale_Order (abs Z, Db * Tb, Qb);
   end Lemma_Numerator_Product;

   procedure Lemma_Expand (A, B, X, Kd, Ks, Kg : Real)
   with Ghost, Post => A * ((B * Kd) * (Ks * X + Kg)) = (Kd * Ks) * ((A * B) * X) + (Kd * Kg) * (A * B);

   procedure Lemma_Expand (A, B, X, Kd, Ks, Kg : Real) is
   begin
      pragma Assert ((B * Kd) * (Ks * X + Kg) = (Kd * Ks) * (B * X) + (Kd * Kg) * B);
      pragma Assert (A * ((Kd * Ks) * (B * X)) = (Kd * Ks) * ((A * B) * X));
      pragma Assert (A * ((Kd * Kg) * B) = (Kd * Kg) * (A * B));
   end Lemma_Expand;

   procedure Lemma_Dust_Expansion (A, B, X, G : Real)
   with
     Ghost,
     Pre  =>
       A >= 0.0
       and then B >= 0.0
       and then X >= 0.0
       and then A * B <= Scale_Bound
       and then G >= G_Factor * ((A * B) * X) + Padding_Left,
     Post => A * ((B * Denominator_Factor) * ((1.0 + Slack) * X + Gap_Dust)) <= G;
   --  The product of the three bounds: (1 + Slack)·Denominator_Factor is less than G_Factor, and Gap_Dust as
   --  the scale amplifies it is far below what is left of the padding.

   procedure Lemma_Dust_Expansion (A, B, X, G : Real) is
      AB  : constant Real := A * B;
      ABX : constant Real := AB * X;
   begin
      Lemma_Nonnegative_Product (A, B);
      Lemma_Nonnegative_Product (AB, X);
      Lemma_Expand (A, B, X, Denominator_Factor, 1.0 + Slack, Gap_Dust);
      pragma
        Assert
          (A * ((B * Denominator_Factor) * ((1.0 + Slack) * X + Gap_Dust))
           = (Denominator_Factor * (1.0 + Slack)) * ABX + (Denominator_Factor * Gap_Dust) * AB);
      pragma Assert ((Denominator_Factor * (1.0 + Slack)) * ABX <= G_Factor * ABX);
      pragma Assert ((Denominator_Factor * Gap_Dust) * AB <= Padding_Left);
   end Lemma_Dust_Expansion;

   procedure Lemma_G_Covers (Q, D, P, T, Rv, Qh, Fe, Amp, Dh, Ia, I, C, Rk, Slip, Size, Sz, Csz, Sl, G : Real)
   with
     Ghost,
     Pre  =>
       D >= 1.0
       and then T * D = P
       and then Qh >= 0.0
       and then Amp >= 0.0
       and then Fe >= Factor_Slack * Amp
       and then Dh >= 1.0
       and then Ia >= 0.0
       and then Slip >= 0.0
       and then Size >= 0.0
       and then Sz >= 0.0
       and then Csz >= 0.0
       and then Sl >= 0.0
       and then abs (Qh - Q) <= Factor_Slack * Amp * Qh
       and then abs (Dh - D) <= Ia * (1.0 + Slack) + Denominator_Dust
       and then
         abs (I - T)
         <= (Slip + Small_Allowance_Real * Size + abs I * (Ia + Small_Allowance_Real)) * (1.0 + Slack) + Tangent_Dust
       and then abs (Rk - Rv) <= Term_Slack * Sz + Slope_Slack * Sl + Retained_Dust
       and then abs (C - Rk) <= Epsilon * abs Rk + Eta
       and then Csz >= (1.0 - Epsilon) * Sz - Eta
       and then Exact_Scale (Qh, Fe, Dh, Ia) <= Scale_Bound
       and then
         G >= G_Factor * (Exact_Scale (Qh, Fe, Dh, Ia) * Exact_Bracket (I, C, Slip, Size, Ia, Csz, Sl)) + Padding_Left,
     Post => abs (Q * (P - D * Rv)) <= G;
   --  The whole comparison over the real numbers: exact values first (Q, D, P, T = P/D and the retained
   --  tangent Rv), then what was computed, Rk being the exact sum of the computed retained tangent and its
   --  constant, which C rounds.

   procedure Lemma_G_Covers (Q, D, P, T, Rv, Qh, Fe, Amp, Dh, Ia, I, C, Rk, Slip, Size, Sz, Csz, Sl, G : Real) is
      A  : constant Real := Qh * (1.0 + Fe);
      B  : constant Real := Dh + Ia;
      P4 : constant Real := abs I * (Ia + Small_Allowance_Real);
      X  : constant Real := Exact_Bracket (I, C, Slip, Size, Ia, Csz, Sl);
      Db : constant Real := B * Denominator_Factor;
      Tb : constant Real := (1.0 + Slack) * X + Gap_Dust;
   begin
      Lemma_Nonnegative_Product (abs I, Ia + Small_Allowance_Real);
      pragma Assert (X >= 0.0);
      pragma Assert (Exact_Scale (Qh, Fe, Dh, Ia) = A * B);

      --  The tangents
      Lemma_Tangent_Gap (T, Rv, I, C, Rk, Slip, Small_Allowance_Real * Size, P4, Sz, Csz, Sl);
      pragma Assert (abs (T - Rv) <= Tb);

      --  Q
      Lemma_Scale_Order (Factor_Slack * Amp, Fe, Qh);
      pragma Assert (abs (Qh - Q) <= Qh * Fe);
      pragma Assert (A = Qh + Qh * Fe);
      pragma Assert (abs Q <= A);
      pragma Assert (A >= 0.0);

      --  D
      pragma Assert (B >= 1.0);
      pragma Assert (D <= B * (1.0 + Slack) + Denominator_Dust);
      pragma Assert (Db = B * (1.0 + Slack) + B * Denominator_Dust);
      pragma Assert (D <= Db);

      Lemma_Numerator_Product (Q, D, P, T, Rv, A, Db, Tb);
      Lemma_Dust_Expansion (A, B, X, G);
   end Lemma_G_Covers;

   procedure Lemma_Division_Undone (Quotient, Numerator, Divisor : Real)
   with Ghost, Pre => Divisor /= 0.0 and then Quotient = Numerator / Divisor, Post => Quotient * Divisor = Numerator;

   procedure Lemma_Division_Undone (Quotient, Numerator, Divisor : Real) is null;

   procedure Lemma_Numerator_Error_Is_Covered
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Axis              : Axis_Name;
      Ideal             : Ideal_Side.Ideal_Node;
      Ideal_Part        : Ideal_Side.Ideal_Axis;
      Retained          : Retained_Side.Retained_Node;
      Factor_Error      : Dimensionless;
      Cached            : Dimensionless;
      Cached_Size       : Dimensionless;
      Nodal_G           : Dimensionless;
      X                 : Real)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Prepared.Active (Axis)
       and then Ideal_Side.Is_Axis_Bounded (Frame, Axis)
       and then Ideal_Side.Is_Node_Bounded (Prepared.Delta_Y, Ideal)
       and then Ideal.Ideal_Q >= 1.0
       and then Ideal.Ideal_Allowance >= 0.0
       and then Ideal.Ideal_Allowance <= 1.0
       and then R (Ideal_Part.Ideal) = R (Ideal_Side.At_Axis (Frame, Axis, Prepared.Delta_Y, Ideal).Ideal)
       and then
         R (Ideal_Part.Numerator_Slip) = R (Ideal_Side.At_Axis (Frame, Axis, Prepared.Delta_Y, Ideal).Numerator_Slip)
       and then
         R (Ideal_Part.Numerator_Size) = R (Ideal_Side.At_Axis (Frame, Axis, Prepared.Delta_Y, Ideal).Numerator_Size)
       and then Ideal_Part.Numerator_Slip >= 0.0
       and then Ideal_Part.Numerator_Size >= 0.0
       and then Retained.Cached_Q >= 0.0
       and then Retained.Amplification >= 0.0
       and then R (Factor_Error) >= Factor_Slack * R (Retained.Amplification)
       and then Retained.Retained_Size (Axis) >= 0.0
       and then Cached_Size >= 0.0
       and then Prepared.Slope (Axis) >= 0.0
       and then
         abs (R (Cached) - (R (Retained.Retained (Axis)) + R (Cache.Constant_Tangent (Axis))))
         <= Epsilon * abs (R (Retained.Retained (Axis)) + R (Cache.Constant_Tangent (Axis))) + Eta
       and then R (Cached_Size) >= (1.0 - Epsilon) * R (Retained.Retained_Size (Axis)) - Eta
       and then
         Exact_Scale (R (Retained.Cached_Q), R (Factor_Error), R (Ideal.Ideal_Q), R (Ideal.Ideal_Allowance))
         <= Scale_Bound
       and then
         R (Nodal_G)
         >= G_Factor
            * (Exact_Scale (R (Retained.Cached_Q), R (Factor_Error), R (Ideal.Ideal_Q), R (Ideal.Ideal_Allowance))
               * Exact_Bracket
                   (R (Ideal_Part.Ideal),
                    R (Cached),
                    R (Ideal_Part.Numerator_Slip),
                    R (Ideal_Part.Numerator_Size),
                    R (Ideal.Ideal_Allowance),
                    R (Cached_Size),
                    R (Prepared.Slope (Axis))))
            + Padding_Left
       and then
         (if Accurate
          then Stages_Are_Accurate (Coefficients, Warp, Cache, Denominator_Scale, Prepared, Ideal, Retained, X)),
     Post =>
       (if Accurate
        then abs Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, X) <= R (Nodal_G));
   --  The two stages are accurate at an accurate sample, and then Nodal_G is at least |g| there on an active
   --  axis. Ideal_Part is what Ideal_Side.At_Axis returned for the axis.

   procedure Lemma_Numerator_Error_Is_Covered
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Axis              : Axis_Name;
      Ideal             : Ideal_Side.Ideal_Node;
      Ideal_Part        : Ideal_Side.Ideal_Axis;
      Retained          : Retained_Side.Retained_Node;
      Factor_Error      : Dimensionless;
      Cached            : Dimensionless;
      Cached_Size       : Dimensionless;
      Nodal_G           : Dimensionless;
      X                 : Real) is
   begin
      pragma Assert (R (Ideal.Ideal_Q) >= 1.0);
      pragma Assert (R (Ideal.Ideal_Allowance) >= 0.0);
      if Accurate then
         pragma Assert (Ideal_Side.Is_Node_Tight (Coefficients, Prepared.Delta_Y, Ideal, X));
         pragma Assert (Retained_Side.Is_Node_Accurate (Warp, Cache, Denominator_Scale, Prepared, Retained, X));
         Ideal_Side.Lemma_Axis (Frame, Coefficients, Axis, Prepared.Delta_Y, Ideal, X);
         pragma
           Assert
             (abs (R (Ideal_Part.Ideal) - Model.Ideal_Tangent (Frame, Coefficients, Axis, X))
              <= (R (Ideal_Part.Numerator_Slip) + Small_Allowance_Real * R (Ideal_Part.Numerator_Size)
                  + abs R (Ideal_Part.Ideal) * (R (Ideal.Ideal_Allowance) + Small_Allowance_Real))
                 * (1.0 + Slack)
                 + Tangent_Dust);
         pragma
           Assert
             (abs (R (Retained.Retained (Axis))
                   + R (Cache.Constant_Tangent (Axis))
                   - Retained_Tangent.Value (Cache, Axis, Model.Distance (Warp, X)))
              <= Term_Slack * R (Retained.Retained_Size (Axis))
                 + Slope_Slack * R (Prepared.Slope (Axis))
                 + Retained_Dust);
         pragma
           Assert
             (abs (R (Retained.Cached_Q) - Model.Cached_Denominator (Cache, Denominator_Scale, Warp, X))
              <= Factor_Slack * R (Retained.Amplification) * R (Retained.Cached_Q));
         pragma
           Assert
             (abs (R (Ideal.Ideal_Q) - Denominator (Coefficients, X))
              <= R (Ideal.Ideal_Allowance) * (1.0 + Slack) + Denominator_Dust);
         pragma Assert (Denominator (Coefficients, X) >= 1.0);
         Lemma_Division_Undone
           (Model.Ideal_Tangent (Frame, Coefficients, Axis, X),
            Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, X),
            Denominator (Coefficients, X));
         Lemma_G_Covers
           (Q    => Model.Cached_Denominator (Cache, Denominator_Scale, Warp, X),
            D    => Denominator (Coefficients, X),
            P    => Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, X),
            T    => Model.Ideal_Tangent (Frame, Coefficients, Axis, X),
            Rv   => Retained_Tangent.Value (Cache, Axis, Model.Distance (Warp, X)),
            Qh   => R (Retained.Cached_Q),
            Fe   => R (Factor_Error),
            Amp  => R (Retained.Amplification),
            Dh   => R (Ideal.Ideal_Q),
            Ia   => R (Ideal.Ideal_Allowance),
            I    => R (Ideal_Part.Ideal),
            C    => R (Cached),
            Rk   => R (Retained.Retained (Axis)) + R (Cache.Constant_Tangent (Axis)),
            Slip => R (Ideal_Part.Numerator_Slip),
            Size => R (Ideal_Part.Numerator_Size),
            Sz   => R (Retained.Retained_Size (Axis)),
            Csz  => R (Cached_Size),
            Sl   => R (Prepared.Slope (Axis)),
            G    => R (Nodal_G));
      end if;
   end Lemma_Numerator_Error_Is_Covered;

   ---------------------------------------------------------------------------
   --  The axes, one by one
   ---------------------------------------------------------------------------

   Axis_Count : constant := 4;

   subtype Axis_Tally is Natural range 0 .. Axis_Count;
   --  How many axes the loop has finished.

   function E_Is_Settled
     (Accurate          : Boolean;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      X                 : Real;
      Bounds            : Node_Bounds) return Boolean
   is (not Bounds.Good
       and then Bounds.Nodal_E >= 0.0
       and then Bounds.Nodal_E <= 1.0
       and then
         (if Accurate
          then abs Model.Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, X) <= R (Bounds.Nodal_E)))
   with Ghost, Pre => Has_Layout (Cache);
   --  What the loop over the axes keeps of the result: it is not yet good, and Nodal_E is within its limits and
   --  covers |e| if the sample is accurate.

   function Others_Are_Kept (Before, After : Axis_Values; Axis : Axis_Name) return Boolean
   is ((if Axis /= X_Axis then R (After (X_Axis)) = R (Before (X_Axis)))
       and then (if Axis /= Y_Axis then R (After (Y_Axis)) = R (Before (Y_Axis)))
       and then (if Axis /= Z_Axis then R (After (Z_Axis)) = R (Before (Z_Axis)))
       and then (if Axis /= E_Axis then R (After (E_Axis)) = R (Before (E_Axis))))
   with Ghost;
   --  Whether the bounds of the axes other than Axis have the values they had.

   function Holds_At
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      X                 : Real;
      Nodal_G           : Axis_Values;
      Tally             : Axis_Tally;
      Other             : Axis_Name) return Boolean
   is (if Axis_Name'Pos (Other) < Tally
       then
         Nodal_G (Other) >= 0.0
         and then Nodal_G (Other) <= Magnitude_Limit
         and then
           (if Accurate and then Prepared.Active (Other)
            then
              abs Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Other, X)
              <= R (Nodal_G (Other)))
       else Nodal_G (Other) = 0.0)
   with Ghost, Pre => Has_Layout (Cache);
   --  What Bounds_At_Node's loop has established about one axis after Tally axes: a finished axis has its bound
   --  within the limits, which covers |g| if the sample is accurate and the axis active, and an axis still to
   --  come has its initial zero.

   function Holds_Until
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      X                 : Real;
      Nodal_G           : Axis_Values;
      Tally             : Axis_Tally) return Boolean
   is (Holds_At (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Tally, X_Axis)
       and then
         Holds_At (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Tally, Y_Axis)
       and then
         Holds_At (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Tally, Z_Axis)
       and then
         Holds_At (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Tally, E_Axis))
   with Ghost, Pre => Has_Layout (Cache);
   --  Holds_At on every axis, written out because the provers handle four facts more readily than a quantifier.

   function Is_Done_Until
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      X                 : Real;
      Nodal_G           : Axis_Values;
      Tally             : Axis_Tally) return Boolean
   with
     Ghost,
     Pre  => Has_Layout (Cache),
     Post =>
       Is_Done_Until'Result
       = Holds_Until (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Tally);
   --  Holds_Until, but not an expression function: the loop carries it as one fact, and only the lemmas below
   --  look inside.

   function Is_Done_Until
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      X                 : Real;
      Nodal_G           : Axis_Values;
      Tally             : Axis_Tally) return Boolean is
   begin
      return Holds_Until (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Tally);
   end Is_Done_Until;

   procedure Lemma_None_Done
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      X                 : Real;
      Nodal_G           : Axis_Values)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Nodal_G (X_Axis) = 0.0
       and then Nodal_G (Y_Axis) = 0.0
       and then Nodal_G (Z_Axis) = 0.0
       and then Nodal_G (E_Axis) = 0.0,
     Post => Is_Done_Until (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, 0);
   --  Before the loop every bound has its initial zero.

   procedure Lemma_None_Done
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      X                 : Real;
      Nodal_G           : Axis_Values) is
   begin
      pragma
        Assert
          (Holds_At (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, 0, X_Axis));
      pragma
        Assert
          (Holds_At (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, 0, Y_Axis));
      pragma
        Assert
          (Holds_At (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, 0, Z_Axis));
      pragma
        Assert
          (Holds_At (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, 0, E_Axis));
      pragma
        Assert (Holds_Until (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, 0));
   end Lemma_None_Done;

   procedure Lemma_Next_Done
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      X                 : Real;
      Before, After     : Axis_Values;
      Axis              : Axis_Name)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then
         Is_Done_Until
           (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Before, Axis_Name'Pos (Axis))
       and then Others_Are_Kept (Before, After, Axis)
       and then
         (if Prepared.Active (Axis)
          then
            After (Axis) >= 0.0
            and then After (Axis) <= Magnitude_Limit
            and then
              (if Accurate
               then
                 abs Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, X)
                 <= R (After (Axis)))
          else R (After (Axis)) = R (Before (Axis))),
     Post =>
       Is_Done_Until
         (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, After, Axis_Name'Pos (Axis) + 1);
   --  One iteration: the other bounds keep their values, and the bound of Axis either was computed, is within
   --  the limits and covers |g|, or keeps its zero because the axis is not active.

   procedure Lemma_Next_Done
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      X                 : Real;
      Before, After     : Axis_Values;
      Axis              : Axis_Name)
   is
      Tally : constant Axis_Tally := Axis_Name'Pos (Axis);
   begin
      pragma
        Assert
          (Holds_Until (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Before, Tally));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Before, Tally, X_Axis));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Before, Tally, Y_Axis));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Before, Tally, Z_Axis));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Before, Tally, E_Axis));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, After, Tally + 1, X_Axis));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, After, Tally + 1, Y_Axis));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, After, Tally + 1, Z_Axis));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, After, Tally + 1, E_Axis));
      pragma
        Assert
          (Holds_Until (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, After, Tally + 1));
   end Lemma_Next_Done;

   procedure Lemma_All_Done
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      X                 : Real;
      Nodal_G           : Axis_Values)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then
         Is_Done_Until
           (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Axis_Count),
     Post =>
       (for all Other in Axis_Name => Nodal_G (Other) >= 0.0 and then Nodal_G (Other) <= Magnitude_Limit)
       and then
         (for all Other in Axis_Name =>
            (if Accurate and then Prepared.Active (Other)
             then
               abs Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Other, X)
               <= R (Nodal_G (Other))));
   --  After the loop every axis is finished.

   procedure Lemma_All_Done
     (Accurate          : Boolean;
      Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      X                 : Real;
      Nodal_G           : Axis_Values)
   is
      Tally : constant Axis_Tally := Axis_Count;
   begin
      pragma
        Assert
          (Holds_Until (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Tally));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Tally, X_Axis));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Tally, Y_Axis));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Tally, Z_Axis));
      pragma
        Assert
          (Holds_At
             (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Nodal_G, Tally, E_Axis));
      pragma
        Assert
          (for all Other in Axis_Name =>
             Other = X_Axis or else Other = Y_Axis or else Other = Z_Axis or else Other = E_Axis);
   end Lemma_All_Done;

   ---------------------------------------------------------------------------
   --  The nodal bounds
   ---------------------------------------------------------------------------

   function Bounds_At_Node
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample) return Node_Bounds
   is
      Result   : Node_Bounds;
      Ideal    : constant Ideal_Side.Ideal_Node := Ideal_Side.At_Node (Coefficients, Prepared.Delta_Y, Sample);
      Retained : constant Retained_Side.Retained_Node :=
        Retained_Side.At_Node (Warp, Cache, Denominator_Scale, Prepared, Sample);
      Accurate : constant Boolean := Sample_Nodes.Is_Accurate (Sample)
      with Ghost;
      X        : constant Real := R (Sample.Node)
      with Ghost;
      Before   : Axis_Values
      with Ghost;
      --  The bounds as an iteration of the loop found them.

      --  The ghost code below cuts what the provers know after every stage, each time to what the rest of the
      --  function needs: the provers take far longer when every rounded operation performed so far is in view.
      --  A cut forgets the tests passed since the previous one as well, so it restates those still needed.
   begin
      Lemma_None_Done (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Result.Nodal_G);
      if not Retained.Good then
         return Result;
      end if;
      Lemma_Factor_Error (Retained.Amplification);

      declare
         Ideal_Q         : Dimensionless renames Ideal.Ideal_Q;
         Ideal_Allowance : Dimensionless renames Ideal.Ideal_Allowance;
         Cached_Q        : Dimensionless renames Retained.Cached_Q;
         Factor_Error    : constant Dimensionless := Factor_Allowance * Retained.Amplification;
         Scale           : Dimensionless;
      begin
         --  A relative error of ε per factor compounds to at most twice their sum while that sum is small;
         --  Factor_Allowance is sixteen times the derived 13u, which includes that doubling. Without a pole Q
         --  is the denominator scale itself, which only this test keeps above Smallest_Partial_Product. It
         --  changes no result: a smaller Q is so far from the ideal one, which is at least one, that Nodal_E
         --  would exceed one below.
         if not (Factor_Error <= Largest_Factor_Error)
           or else not (Cached_Q >= Smallest_Partial_Product and then Cached_Q <= Largest_Cached_Denominator)
           or else not (Ideal_Q <= Largest_Ideal_Denominator)
         then
            return Result;
         end if;
         Lemma_Nodal_E (Cached_Q, Ideal_Q, Factor_Error, Ideal_Allowance);
         Result.Nodal_E :=
           (abs (Cached_Q - Ideal_Q) + Cached_Q * Factor_Error + Ideal_Allowance
            + Small_Allowance * (Cached_Q + Ideal_Q))
           * Rounding_Factor
           + Underflow_Padding;
         pragma
           Assert_And_Cut
             (not Result.Good
              and then
                Is_Done_Until
                  (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Result.Nodal_G, 0)
              and then Result.Nodal_E >= 0.0
              and then R (Result.Nodal_E) >= R (Ideal_Allowance)
              and then
                R (Result.Nodal_E)
                >= E_Factor
                   * (abs (R (Cached_Q) - R (Ideal_Q)) + R (Cached_Q) * R (Factor_Error) + R (Ideal_Allowance))
                   + Half_Padding
              and then Factor_Error <= Largest_Factor_Error
              and then Cached_Q > 0.0
              and then Cached_Q >= Smallest_Partial_Product
              and then Cached_Q <= Largest_Cached_Denominator
              and then Ideal_Q <= Largest_Ideal_Denominator);
         --  A larger value could not prove a positive denominator.
         if not (Result.Nodal_E <= 1.0) then
            return Result;
         end if;

         --  Nodal_E is at least the ideal allowance, so that is at most one as well, which the stages need
         --  for their accuracy; and an accepted factor error is an accepted amplification.
         pragma Assert (R (Ideal_Allowance) <= 1.0);
         pragma Assert (Ideal_Allowance <= 1.0);
         pragma Assert (Retained.Amplification <= Largest_Amplification);
         pragma
           Assert
             (if Accurate
              then Stages_Are_Accurate (Coefficients, Warp, Cache, Denominator_Scale, Prepared, Ideal, Retained, X));
         Lemma_Denominator_Error_Is_Covered
           (Accurate,
            Coefficients,
            Warp,
            Cache,
            Denominator_Scale,
            Prepared,
            Ideal,
            Retained,
            Factor_Error,
            Result.Nodal_E,
            X);
         pragma
           Assert_And_Cut
             (E_Is_Settled (Accurate, Coefficients, Warp, Cache, Denominator_Scale, X, Result)
              and then
                Is_Done_Until
                  (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Result.Nodal_G, 0)
              and then Ideal_Side.Is_Node_Bounded (Prepared.Delta_Y, Ideal)
              and then Ideal_Q >= 1.0
              and then Ideal_Q <= Largest_Ideal_Denominator
              and then Ideal_Allowance >= 0.0
              and then Ideal_Allowance <= 1.0
              and then Cached_Q >= Smallest_Partial_Product
              and then Cached_Q <= Largest_Cached_Denominator
              and then Retained.Amplification >= 16.0
              and then Factor_Error >= 0.0
              and then Factor_Error <= Largest_Factor_Error
              and then R (Factor_Error) >= Factor_Slack * R (Retained.Amplification)
              and then
                (if Accurate
                 then
                   Stages_Are_Accurate (Coefficients, Warp, Cache, Denominator_Scale, Prepared, Ideal, Retained, X)));

         Lemma_Scale (Cached_Q, Factor_Error, Ideal_Q, Ideal_Allowance);
         Scale := (Cached_Q * (1.0 + Factor_Error)) * (Ideal_Q + Ideal_Allowance);
         pragma
           Assert_And_Cut
             (E_Is_Settled (Accurate, Coefficients, Warp, Cache, Denominator_Scale, X, Result)
              and then
                Is_Done_Until
                  (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Result.Nodal_G, 0)
              and then Ideal_Side.Is_Node_Bounded (Prepared.Delta_Y, Ideal)
              and then Ideal_Q >= 1.0
              and then Ideal_Allowance >= 0.0
              and then Ideal_Allowance <= 1.0
              and then Cached_Q >= 0.0
              and then Retained.Amplification >= 0.0
              and then R (Factor_Error) >= Factor_Slack * R (Retained.Amplification)
              and then Scale in 0.0 .. Scale_Limit
              and then Exact_Scale (R (Cached_Q), R (Factor_Error), R (Ideal_Q), R (Ideal_Allowance)) >= 0.0
              and then Exact_Scale (R (Cached_Q), R (Factor_Error), R (Ideal_Q), R (Ideal_Allowance)) <= Scale_Bound
              and then
                R (Scale)
                >= Scale_Factor * Exact_Scale (R (Cached_Q), R (Factor_Error), R (Ideal_Q), R (Ideal_Allowance))
              and then
                (if Accurate
                 then
                   Stages_Are_Accurate (Coefficients, Warp, Cache, Denominator_Scale, Prepared, Ideal, Retained, X)));

         for Axis in Axis_Name loop
            pragma Loop_Invariant (E_Is_Settled (Accurate, Coefficients, Warp, Cache, Denominator_Scale, X, Result));
            pragma
              Loop_Invariant
                (Is_Done_Until
                   (Accurate,
                    Frame,
                    Coefficients,
                    Warp,
                    Cache,
                    Denominator_Scale,
                    Prepared,
                    X,
                    Result.Nodal_G,
                    Axis_Name'Pos (Axis)));
            Before := Result.Nodal_G;
            pragma
              Assert
                (Is_Done_Until
                   (Accurate,
                    Frame,
                    Coefficients,
                    Warp,
                    Cache,
                    Denominator_Scale,
                    Prepared,
                    X,
                    Before,
                    Axis_Name'Pos (Axis)));
            if Prepared.Active (Axis) then
               pragma
                 Assert
                   (abs Retained.Retained (Axis) <= Retained_Side.Largest_Retained_Value
                    and then Retained.Retained_Size (Axis) >= 0.0
                    and then Retained.Retained_Size (Axis) <= Retained_Side.Largest_Retained_Value);
               pragma Assert (abs Cache.Constant_Tangent (Axis) <= Magnitude_Limit);
               pragma Assert (Prepared.Slope (Axis) >= 0.0 and then Prepared.Slope (Axis) <= Poles.Largest_Slope);
               pragma Assert (Ideal_Side.Is_Axis_Bounded (Frame, Axis));
               Lemma_Cached (Retained.Retained (Axis), Cache.Constant_Tangent (Axis), Retained.Retained_Size (Axis));
               declare
                  Ideal_Part  : constant Ideal_Side.Ideal_Axis :=
                    Ideal_Side.At_Axis (Frame, Axis, Prepared.Delta_Y, Ideal);
                  Cached      : constant Dimensionless := Retained.Retained (Axis) + Cache.Constant_Tangent (Axis);
                  Cached_Size : constant Dimensionless :=
                    Retained.Retained_Size (Axis) + abs Cache.Constant_Tangent (Axis);
               begin
                  Lemma_Bracket
                    (Ideal_Part.Ideal,
                     Cached,
                     Ideal_Part.Numerator_Slip,
                     Ideal_Part.Numerator_Size,
                     Ideal_Allowance,
                     Cached_Size,
                     Prepared.Slope (Axis));
                  Lemma_Nodal_G
                    (Scale,
                     abs (Ideal_Part.Ideal - Cached) + Ideal_Part.Numerator_Slip
                     + Small_Allowance * Ideal_Part.Numerator_Size
                     + abs Ideal_Part.Ideal * (Ideal_Allowance + Small_Allowance)
                     + Term_Allowance * Cached_Size
                     + Small_Allowance * Prepared.Slope (Axis)
                     + Small_Allowance * (abs Ideal_Part.Ideal + abs Cached),
                     Exact_Scale (R (Cached_Q), R (Factor_Error), R (Ideal_Q), R (Ideal_Allowance)),
                     Exact_Bracket
                       (R (Ideal_Part.Ideal),
                        R (Cached),
                        R (Ideal_Part.Numerator_Slip),
                        R (Ideal_Part.Numerator_Size),
                        R (Ideal_Allowance),
                        R (Cached_Size),
                        R (Prepared.Slope (Axis))));
                  Result.Nodal_G (Axis) :=
                    Scale
                    * (abs (Ideal_Part.Ideal - Cached) + Ideal_Part.Numerator_Slip
                       + Small_Allowance * Ideal_Part.Numerator_Size
                       + abs Ideal_Part.Ideal * (Ideal_Allowance + Small_Allowance)
                       + Term_Allowance * Cached_Size
                       + Small_Allowance * Prepared.Slope (Axis)
                       + Small_Allowance * (abs Ideal_Part.Ideal + abs Cached))
                    * Rounding_Factor
                    + Underflow_Padding;
                  pragma
                    Assert_And_Cut
                      (E_Is_Settled (Accurate, Coefficients, Warp, Cache, Denominator_Scale, X, Result)
                       and then Others_Are_Kept (Before, Result.Nodal_G, Axis)
                       and then Result.Nodal_G (Axis) >= 0.0
                       and then
                         R (Result.Nodal_G (Axis))
                         >= G_Factor
                            * (Exact_Scale (R (Cached_Q), R (Factor_Error), R (Ideal_Q), R (Ideal_Allowance))
                               * Exact_Bracket
                                   (R (Ideal_Part.Ideal),
                                    R (Cached),
                                    R (Ideal_Part.Numerator_Slip),
                                    R (Ideal_Part.Numerator_Size),
                                    R (Ideal_Allowance),
                                    R (Cached_Size),
                                    R (Prepared.Slope (Axis))))
                            + Padding_Left);
                  if not (Result.Nodal_G (Axis) <= Magnitude_Limit) then
                     return Result;
                  end if;
                  pragma
                    Assert
                      (R (Ideal_Part.Ideal) = R (Ideal_Side.At_Axis (Frame, Axis, Prepared.Delta_Y, Ideal).Ideal));
                  pragma
                    Assert
                      (R (Ideal_Part.Numerator_Slip)
                       = R (Ideal_Side.At_Axis (Frame, Axis, Prepared.Delta_Y, Ideal).Numerator_Slip));
                  pragma
                    Assert
                      (R (Ideal_Part.Numerator_Size)
                       = R (Ideal_Side.At_Axis (Frame, Axis, Prepared.Delta_Y, Ideal).Numerator_Size));
                  Lemma_Numerator_Error_Is_Covered
                    (Accurate          => Accurate,
                     Frame             => Frame,
                     Coefficients      => Coefficients,
                     Warp              => Warp,
                     Cache             => Cache,
                     Denominator_Scale => Denominator_Scale,
                     Prepared          => Prepared,
                     Axis              => Axis,
                     Ideal             => Ideal,
                     Ideal_Part        => Ideal_Part,
                     Retained          => Retained,
                     Factor_Error      => Factor_Error,
                     Cached            => Cached,
                     Cached_Size       => Cached_Size,
                     Nodal_G           => Result.Nodal_G (Axis),
                     X                 => X);
               end;
               pragma
                 Assert_And_Cut
                   (E_Is_Settled (Accurate, Coefficients, Warp, Cache, Denominator_Scale, X, Result)
                    and then Others_Are_Kept (Before, Result.Nodal_G, Axis)
                    and then Result.Nodal_G (Axis) >= 0.0
                    and then Result.Nodal_G (Axis) <= Magnitude_Limit
                    and then
                      (if Accurate
                       then
                         abs Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, X)
                         <= R (Result.Nodal_G (Axis))));
            end if;
            Lemma_Next_Done
              (Accurate,
               Frame,
               Coefficients,
               Warp,
               Cache,
               Denominator_Scale,
               Prepared,
               X,
               Before,
               Result.Nodal_G,
               Axis);
         end loop;
         Lemma_All_Done (Accurate, Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, X, Result.Nodal_G);
         pragma
           Assert_And_Cut
             (Result.Nodal_E >= 0.0
              and then Result.Nodal_E <= 1.0
              and then
                (if Accurate
                 then
                   abs Model.Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, X) <= R (Result.Nodal_E))
              and then
                (for all Other in Axis_Name =>
                   Result.Nodal_G (Other) >= 0.0 and then Result.Nodal_G (Other) <= Magnitude_Limit)
              and then
                (for all Other in Axis_Name =>
                   (if Accurate and then Prepared.Active (Other)
                    then
                      abs Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Other, X)
                      <= R (Result.Nodal_G (Other)))));
      end;
      Result.Good := True;
      pragma Assert (Result.Nodal_E >= 0.0 and then Result.Nodal_E <= 1.0);
      pragma
        Assert
          (for all Axis in Axis_Name =>
             Result.Nodal_G (Axis) >= 0.0 and then Result.Nodal_G (Axis) <= Magnitude_Limit);
      pragma
        Assert
          (if Accurate
           then abs Model.Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, X) <= R (Result.Nodal_E));
      pragma
        Assert
          (for all Axis in Axis_Name =>
             (if Accurate and then Prepared.Active (Axis)
              then
                abs Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, X)
                <= R (Result.Nodal_G (Axis))));
      pragma
        Assert
          (if Sample_Nodes.Is_Accurate (Sample)
           then
             abs Model.Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, R (Sample.Node))
             <= R (Result.Nodal_E));
      pragma
        Assert
          (if Sample_Nodes.Is_Accurate (Sample)
           then
             (for all Axis in Axis_Name =>
                (if Prepared.Active (Axis)
                 then
                   abs Model.Numerator_Error
                         (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, R (Sample.Node))
                   <= R (Result.Nodal_G (Axis)))));
      return Result;
   end Bounds_At_Node;

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Nodes;
