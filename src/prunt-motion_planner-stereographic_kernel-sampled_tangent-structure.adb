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
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

with Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form;
with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed;

package body Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Structure
  with SPARK_Mode => On
is
   use Prunt.Motion_Planner.Stereographic_Kernel.Polynomials;

   --  Throughout, W is the warp, h(V) = W·(1 - V) + V = W + (1 - W)·V the warp scale and U = V/h(V) the
   --  normalized distance. Every polynomial below is built from h, which is linear in V, with the constructors of
   --  Polynomials, so that its value is known from their lemmas alone, without looking at any coefficient.
   --
   --  Q is the scale s times one factor q per pole, linear for a real pole and quadratic for a pair, built by the
   --  same recursion over the poles as the model's products. D and P come from the chart coordinates, which are
   --  polynomials because their Bernstein form is one. That gives e = Q - D.
   --
   --  For g = Q·P - D·(Q·R(U)) the retained tangent R = c + Σ t has to lose its denominators. The term t of a
   --  pole, times that pole's own factor q, is a polynomial n of the degree of q: with U·h = V,
   --
   --     r/(U - p) · (p·h - V)/(p·W) = -r·h/(p·W),
   --     2·(R·(U - A) - I·B)/((U - A)² + B²) · ((V - A·h)² + (B·h)²)/(|π|²·W²)
   --        = 2·(R·(V - A·h) - I·B·h)·h/(|π|²·W²).
   --
   --  So a product Π of factors and the polynomial M = Π·Σ t over the same poles take one more pole as
   --  (Π, M) ↦ (Π·q, M·q + n·Π), which keeps the degree of M within that of Π. This is done for the real poles
   --  and for the pairs separately, and Q·R(U) = s·(c·Πr·Πp + Mr·Πp + Πr·Mp) has degree at most sixteen. No
   --  division by a factor is involved, so the factors need not be shown nonzero; the poles only have to be off
   --  the curve for the terms and the factors to be the quotients they are written as.
   --
   --  The provers follow an equation between two function values into a product, but seldom an equation between
   --  a function value and a sum, product or quotient. Wherever the latter would be needed, the step is a lemma
   --  over plain real numbers whose preconditions are the equations at hand, so that its caller only has to
   --  match them.

   ---------------------------------------------------------------------------
   --  Arithmetic over the reals
   ---------------------------------------------------------------------------

   procedure Lemma_Quotient_Times_Divisor (N, D : Real)
   with Pre => D /= 0.0, Post => (N / D) * D = N;

   procedure Lemma_Quotient_Times_Divisor (N, D : Real) is null;

   procedure Lemma_Is_Quotient (X, N, D : Real)
   with Pre => D /= 0.0 and then X = N / D, Post => X * D = N;
   --  A quotient, multiplied through.

   procedure Lemma_Is_Quotient (X, N, D : Real) is
   begin
      Lemma_Quotient_Times_Divisor (N, D);
   end Lemma_Is_Quotient;

   procedure Lemma_Cancel (X, Y, D : Real)
   with Pre => D /= 0.0 and then X * D = Y * D, Post => X = Y;
   --  A nonzero factor cancels.

   procedure Lemma_Cancel (X, Y, D : Real) is
   begin
      pragma Assert ((X - Y) * D = 0.0);
   end Lemma_Cancel;

   procedure Lemma_Scaled_By_Quotient (X, C, K, D : Real)
   with Pre => D /= 0.0 and then X = (C / D) * K, Post => X * D = C * K;
   --  A multiple by a quotient, multiplied through.

   procedure Lemma_Scaled_By_Quotient (X, C, K, D : Real) is
   begin
      Lemma_Quotient_Times_Divisor (C, D);
      pragma Assert (((C / D) * K) * D = ((C / D) * D) * K);
   end Lemma_Scaled_By_Quotient;

   procedure Lemma_Same_Quotient (X, Y, K, N, D : Real)
   with Pre => D /= 0.0 and then X = (1.0 / D) * K and then K = N and then Y * D = N, Post => X = Y;
   --  A multiple by a reciprocal is the quotient.

   procedure Lemma_Same_Quotient (X, Y, K, N, D : Real) is
   begin
      Lemma_Scaled_By_Quotient (X, 1.0, N, D);
      Lemma_Cancel (X, Y, D);
   end Lemma_Same_Quotient;

   procedure Lemma_Zero_Product (X, Q, S : Real)
   with Pre => X = 0.0 and then S = 0.0, Post => X = Q * S;

   procedure Lemma_Zero_Product (X, Q, S : Real) is null;

   procedure Lemma_Unit_Quotient (N, D : Real)
   with Pre => 0.0 <= N and then N <= D and then D > 0.0, Post => 0.0 <= N / D and then N / D <= 1.0;

   procedure Lemma_Unit_Quotient (N, D : Real) is
   begin
      Lemma_Quotient_Times_Divisor (N, D);
      pragma Assert (if N / D < 0.0 then (N / D) * D < 0.0);
      pragma Assert (if N / D > 1.0 then (N / D) * D > D);
   end Lemma_Unit_Quotient;

   ---------------------------------------------------------------------------
   --  Stored numbers as reals
   ---------------------------------------------------------------------------

   procedure Lemma_Positive (X : Dimensionless)
   with Pre => X > 0.0, Post => R (X) > 0.0;

   procedure Lemma_Positive (X : Dimensionless) is null;

   procedure Lemma_Negative (X : Dimensionless)
   with Pre => X < 0.0, Post => R (X) < 0.0;

   procedure Lemma_Negative (X : Dimensionless) is null;

   procedure Lemma_Above_One (X : Dimensionless)
   with Pre => X > 1.0, Post => R (X) > 1.0;

   procedure Lemma_Above_One (X : Dimensionless) is null;

   procedure Lemma_Zero (X : Dimensionless)
   with Pre => X = 0.0, Post => R (X) = 0.0;

   procedure Lemma_Zero (X : Dimensionless) is null;

   ---------------------------------------------------------------------------
   --  The normalized distance
   ---------------------------------------------------------------------------

   procedure Lemma_Distance (Warp : Dimensionless; V : Real) is
      W : constant Real := R (Warp);
      H : constant Real := Model.Warp_Scale (Warp, V);
   begin
      --  h is a convex combination of W and one, with a positive weight on whichever of them V leaves, and
      --  h - V = W·(1 - V) is not negative.
      Lemma_Positive (Warp);
      pragma Assert (W * (1.0 - V) >= 0.0);
      pragma Assert (H = W * (1.0 - V) + V);
      pragma Assert (if V = 0.0 then H = W);
      pragma Assert (H > 0.0);
      pragma Assert (V <= H);
      Lemma_Unit_Quotient (V, H);
   end Lemma_Distance;

   procedure Lemma_Distance_Times_Scale (Warp : Dimensionless; V : Real)
   with
     Pre  => Warp > 0.0 and then 0.0 <= V and then V <= 1.0,
     Post =>
       Model.Warp_Scale (Warp, V) > 0.0
       and then 0.0 <= Model.Distance (Warp, V)
       and then Model.Distance (Warp, V) <= 1.0
       and then Model.Distance (Warp, V) * Model.Warp_Scale (Warp, V) = V;
   --  U·h = V, which is how the distance enters every identity below.

   procedure Lemma_Distance_Times_Scale (Warp : Dimensionless; V : Real) is
   begin
      Lemma_Distance (Warp, V);
      Lemma_Is_Quotient (Model.Distance (Warp, V), V, Model.Warp_Scale (Warp, V));
   end Lemma_Distance_Times_Scale;

   procedure Lemma_Scale_Of_Warped (W, U, D, X, H : Real)
   with Pre => D = 1.0 - U + W * U and then X * D = W * U and then H = W * (1.0 - X) + X, Post => H * D = W;
   --  With X = W·U/D the warped coordinate, h(X) = W/D.

   procedure Lemma_Scale_Of_Warped (W, U, D, X, H : Real) is
   begin
      pragma Assert (H * D = W * D - W * (X * D) + X * D);
      pragma Assert (W * (X * D) = W * (W * U));
      pragma Assert (W * D = W - W * U + W * (W * U));
   end Lemma_Scale_Of_Warped;

   procedure Lemma_Distance_Of_Warped (Warp : Dimensionless; U : Real) is
      W : constant Real := R (Warp);
      D : constant Real := 1.0 - U + W * U;
   begin
      Lemma_Positive (Warp);
      Lemma_Warped_Is_In_Unit_Interval (U, W);
      pragma Assert (W * U >= 0.0);
      pragma Assert (if U < 1.0 then D > 0.0);
      pragma Assert (if U = 1.0 then D = W);
      pragma Assert (D > 0.0);
      declare
         X : constant Real := Warped (U, W);
         H : constant Real := Model.Warp_Scale (Warp, X);
      begin
         pragma Assert (X = W * U / D);
         Lemma_Quotient_Times_Divisor (W * U, D);
         Lemma_Scale_Of_Warped (W, U, D, X, H);
         --  h·D = W is positive and so is D, so h is positive; and (U·h)·D = U·W = X·D.
         pragma Assert (if H <= 0.0 then H * D <= 0.0);
         pragma Assert (H > 0.0);
         pragma Assert ((U * H) * D = U * (H * D));
         Lemma_Cancel (U * H, X, D);
         Lemma_Quotient_Times_Divisor (X, H);
         Lemma_Cancel (X / H, U, H);
      end;
   end Lemma_Distance_Of_Warped;

   ---------------------------------------------------------------------------
   --  The warp scale as a polynomial
   ---------------------------------------------------------------------------

   function Scale_Polynomial (Warp : Dimensionless) return Polynomial
   is (Linear (R (Warp), 1.0 - R (Warp)))
   with Post => Is_Polynomial (Scale_Polynomial'Result) and then Scale_Polynomial'Result'Last = 1;
   --  h.

   procedure Lemma_Scale_Polynomial (Warp : Dimensionless; V : Real)
   with Post => Eval (Scale_Polynomial (Warp), V) = Model.Warp_Scale (Warp, V);

   procedure Lemma_Scale_Polynomial (Warp : Dimensionless; V : Real) is
   begin
      Lemma_Eval_Linear (R (Warp), 1.0 - R (Warp), V);
      pragma Assert (R (Warp) + (1.0 - R (Warp)) * V = R (Warp) * (1.0 - V) + V);
   end Lemma_Scale_Polynomial;

   ---------------------------------------------------------------------------
   --  The factors of the real poles
   ---------------------------------------------------------------------------

   function Real_Offset_Polynomial
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless) return Polynomial
   is (Difference (Scaled (R (Cache.Pole_Slots (Slot).Pole_Component), Scale_Polynomial (Warp)), Linear (0.0, 1.0)))
   with Post => Is_Polynomial (Real_Offset_Polynomial'Result) and then Real_Offset_Polynomial'Result'Last = 1;
   --  p·h(V) - V.

   procedure Lemma_Real_Offset_Polynomial
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless; V : Real)
   with
     Post =>
       Eval (Real_Offset_Polynomial (Cache, Slot, Warp), V)
       = R (Cache.Pole_Slots (Slot).Pole_Component) * Model.Warp_Scale (Warp, V) - V;

   procedure Lemma_Real_Offset_Polynomial
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless; V : Real)
   is
      P : constant Real := R (Cache.Pole_Slots (Slot).Pole_Component);
   begin
      Lemma_Scale_Polynomial (Warp, V);
      Lemma_Eval_Scaled (P, Scale_Polynomial (Warp), V);
      Lemma_Eval_Linear (0.0, 1.0, V);
      Lemma_Eval_Difference (Scaled (P, Scale_Polynomial (Warp)), Linear (0.0, 1.0), V);
   end Lemma_Real_Offset_Polynomial;

   function Real_Factor_Polynomial
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless) return Polynomial
   is (if R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp) = 0.0
       then Constant_Polynomial (1.0)
       else
         Scaled
           (1.0 / (R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp)), Real_Offset_Polynomial (Cache, Slot, Warp)))
   with Post => Is_Polynomial (Real_Factor_Polynomial'Result) and then Real_Factor_Polynomial'Result'Last <= 1;
   --  The factor of one real pole, as Model.Real_Factor defines it.

   procedure Lemma_Real_Factor_Unfolded
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless; V : Real)
   with
     Pre  => R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp) /= 0.0,
     Post =>
       Model.Real_Factor (Cache, Slot, Warp, V) * (R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp))
       = R (Cache.Pole_Slots (Slot).Pole_Component) * Model.Warp_Scale (Warp, V) - V;
   --  The factor of a real pole which is not zero, multiplied through.

   procedure Lemma_Real_Factor_Unfolded
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless; V : Real) is
   begin
      Lemma_Is_Quotient
        (Model.Real_Factor (Cache, Slot, Warp, V),
         R (Cache.Pole_Slots (Slot).Pole_Component) * Model.Warp_Scale (Warp, V) - V,
         R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp));
   end Lemma_Real_Factor_Unfolded;

   procedure Lemma_Real_Factor_Polynomial
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless; V : Real)
   with Post => Eval (Real_Factor_Polynomial (Cache, Slot, Warp), V) = Model.Real_Factor (Cache, Slot, Warp, V);

   procedure Lemma_Real_Factor_Polynomial
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless; V : Real)
   is
      D : constant Real := R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp);
   begin
      if D = 0.0 then
         Lemma_Eval_Constant (1.0, V);
      else
         Lemma_Real_Offset_Polynomial (Cache, Slot, Warp, V);
         Lemma_Eval_Scaled (1.0 / D, Real_Offset_Polynomial (Cache, Slot, Warp), V);
         Lemma_Real_Factor_Unfolded (Cache, Slot, Warp, V);
         Lemma_Same_Quotient
           (X => Eval (Real_Factor_Polynomial (Cache, Slot, Warp), V),
            Y => Model.Real_Factor (Cache, Slot, Warp, V),
            K => Eval (Real_Offset_Polynomial (Cache, Slot, Warp), V),
            N => R (Cache.Pole_Slots (Slot).Pole_Component) * Model.Warp_Scale (Warp, V) - V,
            D => D);
      end if;
   end Lemma_Real_Factor_Polynomial;

   function Real_Product_Polynomial
     (Cache : Rational_Antiderivative; Count : Natural; Warp : Dimensionless) return Polynomial
   is (if Count = 0
       then Constant_Polynomial (1.0)
       else Product (Real_Product_Polynomial (Cache, Count - 1, Warp), Real_Factor_Polynomial (Cache, Count, Warp)))
   with
     Pre                => Count <= Cache.Real_Pole_Count,
     Post               =>
       Is_Polynomial (Real_Product_Polynomial'Result) and then Real_Product_Polynomial'Result'Last <= Count,
     Subprogram_Variant => (Decreases => Count);
   --  The product of the factors of the first Count real poles.

   procedure Lemma_Real_Product_Polynomial
     (Cache : Rational_Antiderivative; Count : Natural; Warp : Dimensionless; V : Real)
   with
     Pre                => Count <= Cache.Real_Pole_Count,
     Post               =>
       Eval (Real_Product_Polynomial (Cache, Count, Warp), V) = Model.Real_Product (Cache, Count, Warp, V),
     Subprogram_Variant => (Decreases => Count);

   procedure Lemma_Real_Product_Polynomial
     (Cache : Rational_Antiderivative; Count : Natural; Warp : Dimensionless; V : Real) is
   begin
      if Count = 0 then
         Lemma_Eval_Constant (1.0, V);
      else
         Lemma_Real_Product_Polynomial (Cache, Count - 1, Warp, V);
         Lemma_Real_Factor_Polynomial (Cache, Count, Warp, V);
         Lemma_Eval_Product
           (Real_Product_Polynomial (Cache, Count - 1, Warp), Real_Factor_Polynomial (Cache, Count, Warp), V);
      end if;
   end Lemma_Real_Product_Polynomial;

   ---------------------------------------------------------------------------
   --  The factors of the pairs
   ---------------------------------------------------------------------------

   function Pair_Offset_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless) return Polynomial
   is (Difference
         (Linear (0.0, 1.0),
          Scaled (R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component), Scale_Polynomial (Warp))))
   with
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post => Is_Polynomial (Pair_Offset_Polynomial'Result) and then Pair_Offset_Polynomial'Result'Last = 1;
   --  V - A·h(V).

   procedure Lemma_Pair_Offset_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; V : Real)
   with
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post =>
       Eval (Pair_Offset_Polynomial (Cache, Pair, Warp), V)
       = V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V);

   procedure Lemma_Pair_Offset_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; V : Real)
   is
      A : constant Real := R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component);
   begin
      Lemma_Scale_Polynomial (Warp, V);
      Lemma_Eval_Scaled (A, Scale_Polynomial (Warp), V);
      Lemma_Eval_Linear (0.0, 1.0, V);
      Lemma_Eval_Difference (Linear (0.0, 1.0), Scaled (A, Scale_Polynomial (Warp)), V);
   end Lemma_Pair_Offset_Polynomial;

   function Pair_Height_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless) return Polynomial
   is (Scaled (R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component), Scale_Polynomial (Warp)))
   with
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post => Is_Polynomial (Pair_Height_Polynomial'Result) and then Pair_Height_Polynomial'Result'Last = 1;
   --  B·h(V).

   procedure Lemma_Pair_Height_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; V : Real)
   with
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post =>
       Eval (Pair_Height_Polynomial (Cache, Pair, Warp), V)
       = R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V);

   procedure Lemma_Pair_Height_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; V : Real) is
   begin
      Lemma_Scale_Polynomial (Warp, V);
      Lemma_Eval_Scaled
        (R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component), Scale_Polynomial (Warp), V);
   end Lemma_Pair_Height_Polynomial;

   function Pair_Square_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless) return Polynomial
   is (Sum
         (Product (Pair_Offset_Polynomial (Cache, Pair, Warp), Pair_Offset_Polynomial (Cache, Pair, Warp)),
          Product (Pair_Height_Polynomial (Cache, Pair, Warp), Pair_Height_Polynomial (Cache, Pair, Warp))))
   with
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post => Is_Polynomial (Pair_Square_Polynomial'Result) and then Pair_Square_Polynomial'Result'Last = 2;
   --  |V - π·h(V)|² = (V - A·h(V))² + (B·h(V))².

   procedure Lemma_Sum_Of_Squares (Square, X, Y, L1, L2 : Real)
   with Pre => Square = X * X + Y * Y and then X = L1 and then Y = L2, Post => Square = L1 * L1 + L2 * L2;

   procedure Lemma_Sum_Of_Squares (Square, X, Y, L1, L2 : Real) is null;

   procedure Lemma_Pair_Square_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; V : Real)
   with
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post =>
       Eval (Pair_Square_Polynomial (Cache, Pair, Warp), V)
       = (V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V))
         * (V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V))
         + (R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V))
           * (R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V));

   procedure Lemma_Pair_Square_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; V : Real) is
   begin
      Lemma_Pair_Offset_Polynomial (Cache, Pair, Warp, V);
      Lemma_Pair_Height_Polynomial (Cache, Pair, Warp, V);
      Lemma_Eval_Product (Pair_Offset_Polynomial (Cache, Pair, Warp), Pair_Offset_Polynomial (Cache, Pair, Warp), V);
      Lemma_Eval_Product (Pair_Height_Polynomial (Cache, Pair, Warp), Pair_Height_Polynomial (Cache, Pair, Warp), V);
      Lemma_Eval_Sum
        (Product (Pair_Offset_Polynomial (Cache, Pair, Warp), Pair_Offset_Polynomial (Cache, Pair, Warp)),
         Product (Pair_Height_Polynomial (Cache, Pair, Warp), Pair_Height_Polynomial (Cache, Pair, Warp)),
         V);
      Lemma_Sum_Of_Squares
        (Square => Eval (Pair_Square_Polynomial (Cache, Pair, Warp), V),
         X      => Eval (Pair_Offset_Polynomial (Cache, Pair, Warp), V),
         Y      => Eval (Pair_Height_Polynomial (Cache, Pair, Warp), V),
         L1     => V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V),
         L2     =>
           R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V));
   end Lemma_Pair_Square_Polynomial;

   function Pair_Factor_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless) return Polynomial
   is (if Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp)) = 0.0
       then Constant_Polynomial (1.0)
       else
         Scaled
           (1.0 / (Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp))), Pair_Square_Polynomial (Cache, Pair, Warp)))
   with
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post => Is_Polynomial (Pair_Factor_Polynomial'Result) and then Pair_Factor_Polynomial'Result'Last <= 2;
   --  The factor of one pair, as Model.Pair_Factor defines it.

   procedure Lemma_Pair_Factor_Unfolded
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; V : Real)
   with
     Pre  =>
       Has_Layout (Cache)
       and then Pair <= Cache.Pair_Count
       and then Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp)) /= 0.0,
     Post =>
       Model.Pair_Factor (Cache, Pair, Warp, V) * (Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp)))
       = (V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V))
         * (V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V))
         + (R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V))
           * (R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V));
   --  The factor of a pair which is not zero, multiplied through.

   procedure Lemma_Pair_Factor_Unfolded
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; V : Real)
   is
      L1 : constant Real :=
        V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V);
      L2 : constant Real :=
        R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V);
   begin
      Lemma_Is_Quotient
        (Model.Pair_Factor (Cache, Pair, Warp, V),
         L1 * L1 + L2 * L2,
         Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp)));
   end Lemma_Pair_Factor_Unfolded;

   procedure Lemma_Pair_Factor_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; V : Real)
   with
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post => Eval (Pair_Factor_Polynomial (Cache, Pair, Warp), V) = Model.Pair_Factor (Cache, Pair, Warp, V);

   procedure Lemma_Pair_Factor_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; V : Real)
   is
      D  : constant Real := Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp));
      L1 : constant Real :=
        V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V);
      L2 : constant Real :=
        R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V);
   begin
      if D = 0.0 then
         Lemma_Eval_Constant (1.0, V);
      else
         Lemma_Pair_Square_Polynomial (Cache, Pair, Warp, V);
         Lemma_Eval_Scaled (1.0 / D, Pair_Square_Polynomial (Cache, Pair, Warp), V);
         Lemma_Pair_Factor_Unfolded (Cache, Pair, Warp, V);
         Lemma_Same_Quotient
           (X => Eval (Pair_Factor_Polynomial (Cache, Pair, Warp), V),
            Y => Model.Pair_Factor (Cache, Pair, Warp, V),
            K => Eval (Pair_Square_Polynomial (Cache, Pair, Warp), V),
            N => L1 * L1 + L2 * L2,
            D => D);
      end if;
   end Lemma_Pair_Factor_Polynomial;

   function Pair_Product_Polynomial
     (Cache : Rational_Antiderivative; Count : Natural; Warp : Dimensionless) return Polynomial
   is (if Count = 0
       then Constant_Polynomial (1.0)
       else Product (Pair_Product_Polynomial (Cache, Count - 1, Warp), Pair_Factor_Polynomial (Cache, Count, Warp)))
   with
     Pre                => Has_Layout (Cache) and then Count <= Cache.Pair_Count,
     Post               =>
       Is_Polynomial (Pair_Product_Polynomial'Result) and then Pair_Product_Polynomial'Result'Last <= 2 * Count,
     Subprogram_Variant => (Decreases => Count);
   --  The product of the factors of the first Count pairs.

   procedure Lemma_Pair_Product_Polynomial
     (Cache : Rational_Antiderivative; Count : Natural; Warp : Dimensionless; V : Real)
   with
     Pre                => Has_Layout (Cache) and then Count <= Cache.Pair_Count,
     Post               =>
       Eval (Pair_Product_Polynomial (Cache, Count, Warp), V) = Model.Pair_Product (Cache, Count, Warp, V),
     Subprogram_Variant => (Decreases => Count);

   procedure Lemma_Pair_Product_Polynomial
     (Cache : Rational_Antiderivative; Count : Natural; Warp : Dimensionless; V : Real) is
   begin
      if Count = 0 then
         Lemma_Eval_Constant (1.0, V);
      else
         Lemma_Pair_Product_Polynomial (Cache, Count - 1, Warp, V);
         Lemma_Pair_Factor_Polynomial (Cache, Count, Warp, V);
         Lemma_Eval_Product
           (Pair_Product_Polynomial (Cache, Count - 1, Warp), Pair_Factor_Polynomial (Cache, Count, Warp), V);
      end if;
   end Lemma_Pair_Product_Polynomial;

   ---------------------------------------------------------------------------
   --  The cached denominator Q
   ---------------------------------------------------------------------------

   function Pole_Product_Polynomial (Cache : Rational_Antiderivative; Warp : Dimensionless) return Polynomial
   is (Product
         (Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp),
          Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp)))
   with
     Pre  => Has_Layout (Cache),
     Post =>
       Is_Polynomial (Pole_Product_Polynomial'Result)
       and then Pole_Product_Polynomial'Result'Last <= Maximum_Rational_Degree;
   --  The product of the factors of every pole: Q without its scale.

   function Cached_Denominator_Polynomial
     (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless) return Polynomial
   is (Scaled (R (Denominator_Scale), Pole_Product_Polynomial (Cache, Warp)))
   with
     Pre  => Has_Layout (Cache),
     Post =>
       Is_Polynomial (Cached_Denominator_Polynomial'Result)
       and then Cached_Denominator_Polynomial'Result'Last <= Maximum_Rational_Degree;
   --  Q.

   procedure Lemma_Scaled_Product (X, Y, S, A, B : Real)
   with Pre => X = S * Y and then Y = A * B, Post => X = S * A * B;

   procedure Lemma_Scaled_Product (X, Y, S, A, B : Real) is null;

   procedure Lemma_Pole_Product_Polynomial (Cache : Rational_Antiderivative; Warp : Dimensionless; V : Real)
   with
     Pre  => Has_Layout (Cache),
     Post =>
       Eval (Pole_Product_Polynomial (Cache, Warp), V)
       = Model.Real_Product (Cache, Cache.Real_Pole_Count, Warp, V)
         * Model.Pair_Product (Cache, Cache.Pair_Count, Warp, V);

   procedure Lemma_Pole_Product_Polynomial (Cache : Rational_Antiderivative; Warp : Dimensionless; V : Real) is
   begin
      Lemma_Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp, V);
      Lemma_Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp, V);
      Lemma_Eval_Product
        (Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp),
         Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp),
         V);
   end Lemma_Pole_Product_Polynomial;

   procedure Lemma_Cached_Denominator_Polynomial
     (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless; V : Real)
   with
     Pre  => Has_Layout (Cache),
     Post =>
       Eval (Cached_Denominator_Polynomial (Cache, Denominator_Scale, Warp), V)
       = Model.Cached_Denominator (Cache, Denominator_Scale, Warp, V);

   procedure Lemma_Cached_Denominator_Polynomial
     (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless; V : Real) is
   begin
      Lemma_Pole_Product_Polynomial (Cache, Warp, V);
      Lemma_Eval_Scaled (R (Denominator_Scale), Pole_Product_Polynomial (Cache, Warp), V);
      Lemma_Scaled_Product
        (X => Eval (Cached_Denominator_Polynomial (Cache, Denominator_Scale, Warp), V),
         Y => Eval (Pole_Product_Polynomial (Cache, Warp), V),
         S => R (Denominator_Scale),
         A => Model.Real_Product (Cache, Cache.Real_Pole_Count, Warp, V),
         B => Model.Pair_Product (Cache, Cache.Pair_Count, Warp, V));
   end Lemma_Cached_Denominator_Polynomial;

   ---------------------------------------------------------------------------
   --  The ideal tangent's denominator D and numerator P
   ---------------------------------------------------------------------------

   function Chart_Polynomial (Coefficients : Bernstein_Chart; Component : Chart_Component_Index) return Polynomial
   is (From_Bernstein (Chart_Controls (Coefficients, Component)))
   with Post => Is_Polynomial (Chart_Polynomial'Result) and then Chart_Polynomial'Result'Last = Fixed_Chart_Degree;
   --  One chart coordinate.

   procedure Lemma_Chart_Polynomial (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; V : Real)
   with Post => Eval (Chart_Polynomial (Coefficients, Component), V) = Chart_Value (Coefficients, Component, V);

   procedure Lemma_Chart_Polynomial (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; V : Real) is
   begin
      Lemma_Eval_From_Bernstein (Chart_Controls (Coefficients, Component), V);
   end Lemma_Chart_Polynomial;

   function Chart_Square_Polynomial
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index) return Polynomial
   is (Product (Chart_Polynomial (Coefficients, Component), Chart_Polynomial (Coefficients, Component)))
   with
     Post =>
       Is_Polynomial (Chart_Square_Polynomial'Result)
       and then Chart_Square_Polynomial'Result'Last = 2 * Fixed_Chart_Degree;
   --  The square of one chart coordinate.

   procedure Lemma_Chart_Square_Polynomial
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; V : Real)
   with
     Post =>
       Eval (Chart_Square_Polynomial (Coefficients, Component), V)
       = Chart_Value (Coefficients, Component, V) * Chart_Value (Coefficients, Component, V);

   procedure Lemma_Chart_Square_Polynomial
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; V : Real) is
   begin
      Lemma_Chart_Polynomial (Coefficients, Component, V);
      Lemma_Eval_Product (Chart_Polynomial (Coefficients, Component), Chart_Polynomial (Coefficients, Component), V);
   end Lemma_Chart_Square_Polynomial;

   function Normal_Square_Polynomial (Coefficients : Bernstein_Chart) return Polynomial
   is (Sum (Chart_Square_Polynomial (Coefficients, 2), Chart_Square_Polynomial (Coefficients, 3)))
   with
     Post =>
       Is_Polynomial (Normal_Square_Polynomial'Result)
       and then Normal_Square_Polynomial'Result'Last = 2 * Fixed_Chart_Degree;
   --  Y₂² + Y₃².

   procedure Lemma_Normal_Square_Polynomial (Coefficients : Bernstein_Chart; V : Real)
   with Post => Eval (Normal_Square_Polynomial (Coefficients), V) = Normal_Square (Coefficients, V);

   procedure Lemma_Normal_Square_Polynomial (Coefficients : Bernstein_Chart; V : Real) is
   begin
      Lemma_Chart_Square_Polynomial (Coefficients, 2, V);
      Lemma_Chart_Square_Polynomial (Coefficients, 3, V);
      Lemma_Eval_Sum (Chart_Square_Polynomial (Coefficients, 2), Chart_Square_Polynomial (Coefficients, 3), V);
   end Lemma_Normal_Square_Polynomial;

   function Ideal_Denominator_Polynomial (Coefficients : Bernstein_Chart) return Polynomial
   is (Sum
         (Sum (Constant_Polynomial (1.0), Chart_Square_Polynomial (Coefficients, 1)),
          Normal_Square_Polynomial (Coefficients)))
   with
     Post =>
       Is_Polynomial (Ideal_Denominator_Polynomial'Result)
       and then Ideal_Denominator_Polynomial'Result'Last = 2 * Fixed_Chart_Degree;
   --  D = 1 + Y·Y.

   procedure Lemma_Ideal_Denominator_Polynomial (Coefficients : Bernstein_Chart; V : Real)
   with Post => Eval (Ideal_Denominator_Polynomial (Coefficients), V) = Denominator (Coefficients, V);

   procedure Lemma_Ideal_Denominator_Polynomial (Coefficients : Bernstein_Chart; V : Real) is
   begin
      Lemma_Eval_Constant (1.0, V);
      Lemma_Chart_Square_Polynomial (Coefficients, 1, V);
      Lemma_Normal_Square_Polynomial (Coefficients, V);
      Lemma_Eval_Sum (Constant_Polynomial (1.0), Chart_Square_Polynomial (Coefficients, 1), V);
      Lemma_Eval_Sum
        (Sum (Constant_Polynomial (1.0), Chart_Square_Polynomial (Coefficients, 1)),
         Normal_Square_Polynomial (Coefficients),
         V);
   end Lemma_Ideal_Denominator_Polynomial;

   function Frame_Numerator_Polynomial
     (Coefficients : Bernstein_Chart; Component : Frame_Component_Index) return Polynomial
   is (if Component = 0
       then
         Difference
           (Difference (Constant_Polynomial (1.0), Chart_Square_Polynomial (Coefficients, 1)),
            Normal_Square_Polynomial (Coefficients))
       else Scaled (2.0, Chart_Polynomial (Coefficients, Component)))
   with
     Post =>
       Is_Polynomial (Frame_Numerator_Polynomial'Result)
       and then Frame_Numerator_Polynomial'Result'Last <= 2 * Fixed_Chart_Degree;
   --  The tangent's numerator along one frame vector: 1 - Y·Y, then 2·Y꜀.

   procedure Lemma_Frame_Numerator_Polynomial
     (Coefficients : Bernstein_Chart; Component : Frame_Component_Index; V : Real)
   with
     Post => Eval (Frame_Numerator_Polynomial (Coefficients, Component), V) = Numerator (Coefficients, Component, V);

   procedure Lemma_Frame_Numerator_Polynomial
     (Coefficients : Bernstein_Chart; Component : Frame_Component_Index; V : Real) is
   begin
      if Component = 0 then
         Lemma_Eval_Constant (1.0, V);
         Lemma_Chart_Square_Polynomial (Coefficients, 1, V);
         Lemma_Normal_Square_Polynomial (Coefficients, V);
         Lemma_Eval_Difference (Constant_Polynomial (1.0), Chart_Square_Polynomial (Coefficients, 1), V);
         Lemma_Eval_Difference
           (Difference (Constant_Polynomial (1.0), Chart_Square_Polynomial (Coefficients, 1)),
            Normal_Square_Polynomial (Coefficients),
            V);
      else
         Lemma_Chart_Polynomial (Coefficients, Component, V);
         Lemma_Eval_Scaled (2.0, Chart_Polynomial (Coefficients, Component), V);
      end if;
   end Lemma_Frame_Numerator_Polynomial;

   function Frame_Term_Polynomial
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; Component : Frame_Component_Index)
      return Polynomial
   is (Scaled (R (Frame (Component) (Axis)), Frame_Numerator_Polynomial (Coefficients, Component)))
   with
     Post =>
       Is_Polynomial (Frame_Term_Polynomial'Result)
       and then Frame_Term_Polynomial'Result'Last <= 2 * Fixed_Chart_Degree;
   --  What one frame vector contributes to the numerator on one machine axis.

   procedure Lemma_Frame_Term_Polynomial
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Axis         : Axis_Name;
      Component    : Frame_Component_Index;
      V            : Real)
   with
     Post =>
       Eval (Frame_Term_Polynomial (Frame, Coefficients, Axis, Component), V)
       = Numerator (Coefficients, Component, V) * R (Frame (Component) (Axis));

   procedure Lemma_Frame_Term_Polynomial
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Axis         : Axis_Name;
      Component    : Frame_Component_Index;
      V            : Real) is
   begin
      Lemma_Frame_Numerator_Polynomial (Coefficients, Component, V);
      Lemma_Eval_Scaled (R (Frame (Component) (Axis)), Frame_Numerator_Polynomial (Coefficients, Component), V);
   end Lemma_Frame_Term_Polynomial;

   function Tangent_Numerator_Polynomial
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name) return Polynomial
   is (Sum
         (Sum
            (Sum
               (Frame_Term_Polynomial (Frame, Coefficients, Axis, 0),
                Frame_Term_Polynomial (Frame, Coefficients, Axis, 1)),
             Frame_Term_Polynomial (Frame, Coefficients, Axis, 2)),
          Frame_Term_Polynomial (Frame, Coefficients, Axis, 3)))
   with
     Post =>
       Is_Polynomial (Tangent_Numerator_Polynomial'Result)
       and then Tangent_Numerator_Polynomial'Result'Last <= 2 * Fixed_Chart_Degree;
   --  P on one machine axis.

   procedure Lemma_Tangent_Numerator_Polynomial
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real)
   with
     Post =>
       Eval (Tangent_Numerator_Polynomial (Frame, Coefficients, Axis), V)
       = Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, V);

   procedure Lemma_Tangent_Numerator_Polynomial
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real) is
   begin
      Lemma_Frame_Term_Polynomial (Frame, Coefficients, Axis, 0, V);
      Lemma_Frame_Term_Polynomial (Frame, Coefficients, Axis, 1, V);
      Lemma_Frame_Term_Polynomial (Frame, Coefficients, Axis, 2, V);
      Lemma_Frame_Term_Polynomial (Frame, Coefficients, Axis, 3, V);
      Lemma_Eval_Sum
        (Frame_Term_Polynomial (Frame, Coefficients, Axis, 0),
         Frame_Term_Polynomial (Frame, Coefficients, Axis, 1),
         V);
      Lemma_Eval_Sum
        (Sum
           (Frame_Term_Polynomial (Frame, Coefficients, Axis, 0),
            Frame_Term_Polynomial (Frame, Coefficients, Axis, 1)),
         Frame_Term_Polynomial (Frame, Coefficients, Axis, 2),
         V);
      Lemma_Eval_Sum
        (Sum
           (Sum
              (Frame_Term_Polynomial (Frame, Coefficients, Axis, 0),
               Frame_Term_Polynomial (Frame, Coefficients, Axis, 1)),
            Frame_Term_Polynomial (Frame, Coefficients, Axis, 2)),
         Frame_Term_Polynomial (Frame, Coefficients, Axis, 3),
         V);
   end Lemma_Tangent_Numerator_Polynomial;

   ---------------------------------------------------------------------------
   --  The denominator error e
   ---------------------------------------------------------------------------

   function Denominator_Error_Polynomial
     (Coefficients      : Bernstein_Chart;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Warp              : Dimensionless) return Polynomials.Polynomial
   is (Difference
         (Cached_Denominator_Polynomial (Cache, Denominator_Scale, Warp),
          Ideal_Denominator_Polynomial (Coefficients)));

   procedure Lemma_Denominator_Error
     (Coefficients      : Bernstein_Chart;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Warp              : Dimensionless;
      V                 : Real) is
   begin
      Lemma_Cached_Denominator_Polynomial (Cache, Denominator_Scale, Warp, V);
      Lemma_Ideal_Denominator_Polynomial (Coefficients, V);
      Lemma_Eval_Difference
        (Cached_Denominator_Polynomial (Cache, Denominator_Scale, Warp),
         Ideal_Denominator_Polynomial (Coefficients),
         V);
   end Lemma_Denominator_Error;

   ---------------------------------------------------------------------------
   --  No pole is on the curve
   ---------------------------------------------------------------------------

   procedure Lemma_Real_Pole_Is_Off
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; U : Real)
   with
     Pre  => Is_Regular (Warp, Cache) and then Slot <= Cache.Real_Pole_Count and then 0.0 <= U and then U <= 1.0,
     Post =>
       R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp) /= 0.0
       and then U /= R (Cache.Pole_Slots (Slot).Pole_Component);
   --  A real pole is neither zero nor at any distance of the curve.

   procedure Lemma_Real_Pole_Is_Off
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; U : Real) is
   begin
      Lemma_Positive (Warp);
      pragma
        Assert (Cache.Pole_Slots (Slot).Pole_Component < 0.0 or else Cache.Pole_Slots (Slot).Pole_Component > 1.0);
      if Cache.Pole_Slots (Slot).Pole_Component < 0.0 then
         Lemma_Negative (Cache.Pole_Slots (Slot).Pole_Component);
         pragma Assert (R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp) < 0.0);
      else
         Lemma_Above_One (Cache.Pole_Slots (Slot).Pole_Component);
         pragma Assert (R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp) > 0.0);
      end if;
   end Lemma_Real_Pole_Is_Off;

   procedure Lemma_Pair_Pole_Is_Off
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; U : Real)
   with
     Pre  => Is_Regular (Warp, Cache) and then Pair <= Cache.Pair_Count,
     Post =>
       Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp)) > 0.0
       and then Pair_Distance_Square (Cache, Pair, U) > 0.0;
   --  A pair is off the real axis, so it is neither zero nor at any real distance.

   procedure Lemma_Pair_Pole_Is_Off
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; U : Real)
   is
      W : constant Real := R (Warp);
      A : constant Real := R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component);
      B : constant Real := R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component);
   begin
      Lemma_Positive (Warp);
      pragma Assert (Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair).Pole_Component > 0.0);
      Lemma_Positive (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component);
      pragma Assert (B * B > 0.0);
      pragma Assert (A * A >= 0.0);
      pragma Assert ((U - A) * (U - A) >= 0.0);
      pragma Assert (W * W > 0.0);
      pragma Assert (Model.Pair_Norm (Cache, Pair) = A * A + B * B);
      pragma Assert ((A * A + B * B) * (W * W) > 0.0);
      pragma Assert (Pair_Distance_Square (Cache, Pair, U) = (U - A) * (U - A) + B * B);
   end Lemma_Pair_Pole_Is_Off;

   ---------------------------------------------------------------------------
   --  Adding one term to a sum over a common denominator
   ---------------------------------------------------------------------------

   procedure Lemma_Add_Term (X, Y, A1, A2, Q, M, F, N, S, T, Total : Real)
   with
     Pre  =>
       X = A1 + A2
       and then A1 = M * F
       and then A2 = N * Q
       and then Y = Q * F
       and then M = Q * S
       and then N = T * F
       and then Total = S + T,
     Post => X = Y * Total;
   --  If M/Q = S and N/F = T, then (M·F + N·Q)/(Q·F) = S + T, without the divisions.

   procedure Lemma_Add_Term (X, Y, A1, A2, Q, M, F, N, S, T, Total : Real) is
   begin
      pragma Assert (M * F = (Q * F) * S);
      pragma Assert (N * Q = (Q * F) * T);
      pragma Assert ((Q * F) * S + (Q * F) * T = (Q * F) * (S + T));
   end Lemma_Add_Term;

   procedure Lemma_Add_Factor (Product_Before, Sum_Before, Factor, Term : Polynomial; V, S, T, Total : Real)
   with
     Pre  =>
       Is_Polynomial (Product_Before)
       and then Is_Polynomial (Sum_Before)
       and then Is_Polynomial (Factor)
       and then Is_Polynomial (Term)
       and then Product_Before'Last <= Maximum_Rational_Degree
       and then Sum_Before'Last <= Maximum_Rational_Degree
       and then Factor'Last <= 2
       and then Term'Last <= 2
       and then Eval (Sum_Before, V) = Eval (Product_Before, V) * S
       and then Eval (Term, V) = T * Eval (Factor, V)
       and then Total = S + T,
     Post =>
       Eval (Sum (Product (Sum_Before, Factor), Product (Term, Product_Before)), V)
       = Eval (Product (Product_Before, Factor), V) * Total;
   --  The step of the two inductions below: a product of factors, and a polynomial which is that product times a
   --  sum S, take one more factor, whose term T of the sum is the polynomial Term over that factor.

   procedure Lemma_Add_Factor (Product_Before, Sum_Before, Factor, Term : Polynomial; V, S, T, Total : Real) is
   begin
      Lemma_Eval_Product (Sum_Before, Factor, V);
      Lemma_Eval_Product (Term, Product_Before, V);
      Lemma_Eval_Product (Product_Before, Factor, V);
      Lemma_Eval_Sum (Product (Sum_Before, Factor), Product (Term, Product_Before), V);
      Lemma_Add_Term
        (X     => Eval (Sum (Product (Sum_Before, Factor), Product (Term, Product_Before)), V),
         Y     => Eval (Product (Product_Before, Factor), V),
         A1    => Eval (Product (Sum_Before, Factor), V),
         A2    => Eval (Product (Term, Product_Before), V),
         Q     => Eval (Product_Before, V),
         M     => Eval (Sum_Before, V),
         F     => Eval (Factor, V),
         N     => Eval (Term, V),
         S     => S,
         T     => T,
         Total => Total);
   end Lemma_Add_Factor;

   ---------------------------------------------------------------------------
   --  The terms of the real poles over their factors
   ---------------------------------------------------------------------------

   function Real_Residue_Polynomial
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless; Axis : Axis_Name)
      return Polynomial
   is (if R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp) = 0.0
       then Constant_Polynomial (0.0)
       else
         Scaled
           ((-R (Cache.Pole_Slots (Slot).Residue_Component (Axis)))
            / (R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp)),
            Scale_Polynomial (Warp)))
   with Post => Is_Polynomial (Real_Residue_Polynomial'Result) and then Real_Residue_Polynomial'Result'Last <= 1;
   --  -r·h(V)/(p·W): the term r/(U - p) of one real pole times that pole's factor.

   procedure Lemma_Real_Residue_Value
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless; Axis : Axis_Name; V : Real)
   with
     Pre  => R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp) /= 0.0,
     Post =>
       Eval (Real_Residue_Polynomial (Cache, Slot, Warp, Axis), V)
       * (R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp))
       = (-R (Cache.Pole_Slots (Slot).Residue_Component (Axis))) * Model.Warp_Scale (Warp, V);
   --  The value of that polynomial, multiplied through.

   procedure Lemma_Real_Residue_Value
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless; Axis : Axis_Name; V : Real)
   is
      Residue : constant Real := R (Cache.Pole_Slots (Slot).Residue_Component (Axis));
      D       : constant Real := R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp);
   begin
      Lemma_Scale_Polynomial (Warp, V);
      Lemma_Eval_Scaled ((-Residue) / D, Scale_Polynomial (Warp), V);
      Lemma_Scaled_By_Quotient
        (X => Eval (Real_Residue_Polynomial (Cache, Slot, Warp, Axis), V),
         C => -Residue,
         K => Eval (Scale_Polynomial (Warp), V),
         D => D);
   end Lemma_Real_Residue_Value;

   procedure Lemma_Real_Term_Unfolded
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Axis : Axis_Name; U : Real)
   with
     Pre  => U /= R (Cache.Pole_Slots (Slot).Pole_Component),
     Post =>
       Real_Term (Cache, Slot, Axis, U) * (U - R (Cache.Pole_Slots (Slot).Pole_Component))
       = R (Cache.Pole_Slots (Slot).Residue_Component (Axis));
   --  The term of a real pole away from the pole, multiplied through.

   procedure Lemma_Real_Term_Unfolded
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Axis : Axis_Name; U : Real) is
   begin
      Lemma_Is_Quotient
        (Real_Term (Cache, Slot, Axis, U),
         R (Cache.Pole_Slots (Slot).Residue_Component (Axis)),
         U - R (Cache.Pole_Slots (Slot).Pole_Component));
   end Lemma_Real_Term_Unfolded;

   procedure Lemma_Real_Algebra (Residue, Pole, H, V, U, D, T, Q, N : Real)
   with
     Pre  =>
       U * H = V
       and then D /= 0.0
       and then T * (U - Pole) = Residue
       and then Q * D = Pole * H - V
       and then N * D = (-Residue) * H,
     Post => N = T * Q;
   --  With T = r/(U - p) the term and Q = (p·h - V)/D the factor: p·h - V = -(U - p)·h, so T·Q·D = -r·h.

   procedure Lemma_Real_Algebra (Residue, Pole, H, V, U, D, T, Q, N : Real) is
   begin
      pragma Assert (Pole * H - V = -((U - Pole) * H));
      pragma Assert (T * (Q * D) = -(T * ((U - Pole) * H)));
      pragma Assert (T * ((U - Pole) * H) = (T * (U - Pole)) * H);
      pragma Assert ((T * Q) * D = -(Residue * H));
      Lemma_Cancel (N, T * Q, D);
   end Lemma_Real_Algebra;

   procedure Lemma_Real_Residue_Polynomial
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Axis : Axis_Name; V : Real)
   with
     Pre  => Is_Regular (Warp, Cache) and then Slot <= Cache.Real_Pole_Count and then 0.0 <= V and then V <= 1.0,
     Post =>
       Eval (Real_Residue_Polynomial (Cache, Slot, Warp, Axis), V)
       = Real_Term (Cache, Slot, Axis, Model.Distance (Warp, V)) * Model.Real_Factor (Cache, Slot, Warp, V);

   procedure Lemma_Real_Residue_Polynomial
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Axis : Axis_Name; V : Real)
   is
      U : constant Real := Model.Distance (Warp, V);
   begin
      Lemma_Distance_Times_Scale (Warp, V);
      Lemma_Real_Pole_Is_Off (Warp, Cache, Slot, U);
      Lemma_Real_Term_Unfolded (Cache, Slot, Axis, U);
      Lemma_Real_Factor_Unfolded (Cache, Slot, Warp, V);
      Lemma_Real_Residue_Value (Cache, Slot, Warp, Axis, V);
      Lemma_Real_Algebra
        (Residue => R (Cache.Pole_Slots (Slot).Residue_Component (Axis)),
         Pole    => R (Cache.Pole_Slots (Slot).Pole_Component),
         H       => Model.Warp_Scale (Warp, V),
         V       => V,
         U       => U,
         D       => R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp),
         T       => Real_Term (Cache, Slot, Axis, U),
         Q       => Model.Real_Factor (Cache, Slot, Warp, V),
         N       => Eval (Real_Residue_Polynomial (Cache, Slot, Warp, Axis), V));
   end Lemma_Real_Residue_Polynomial;

   function Real_Sum_Polynomial
     (Cache : Rational_Antiderivative; Count : Natural; Warp : Dimensionless; Axis : Axis_Name) return Polynomial
   is (if Count = 0
       then Constant_Polynomial (0.0)
       else
         Sum
           (Product (Real_Sum_Polynomial (Cache, Count - 1, Warp, Axis), Real_Factor_Polynomial (Cache, Count, Warp)),
            Product
              (Real_Residue_Polynomial (Cache, Count, Warp, Axis), Real_Product_Polynomial (Cache, Count - 1, Warp))))
   with
     Pre                => Count <= Cache.Real_Pole_Count,
     Post               =>
       Is_Polynomial (Real_Sum_Polynomial'Result) and then Real_Sum_Polynomial'Result'Last <= Count,
     Subprogram_Variant => (Decreases => Count);
   --  The product of the factors of the first Count real poles, times the sum of the terms of those poles.

   procedure Lemma_Real_Sum_Polynomial
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Count : Natural; Axis : Axis_Name; V : Real)
   with
     Pre                =>
       Is_Regular (Warp, Cache) and then Count <= Cache.Real_Pole_Count and then 0.0 <= V and then V <= 1.0,
     Post               =>
       Eval (Real_Sum_Polynomial (Cache, Count, Warp, Axis), V)
       = Eval (Real_Product_Polynomial (Cache, Count, Warp), V)
         * Real_Sum (Cache, Count, Axis, Model.Distance (Warp, V)),
     Subprogram_Variant => (Decreases => Count);

   procedure Lemma_Real_Sum_Polynomial
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Count : Natural; Axis : Axis_Name; V : Real)
   is
      U : constant Real := Model.Distance (Warp, V);
   begin
      if Count = 0 then
         Lemma_Eval_Constant (0.0, V);
         Lemma_Zero_Product
           (X => Eval (Real_Sum_Polynomial (Cache, Count, Warp, Axis), V),
            Q => Eval (Real_Product_Polynomial (Cache, Count, Warp), V),
            S => Real_Sum (Cache, Count, Axis, U));
      else
         Lemma_Real_Sum_Polynomial (Warp, Cache, Count - 1, Axis, V);
         Lemma_Real_Factor_Polynomial (Cache, Count, Warp, V);
         Lemma_Real_Residue_Polynomial (Warp, Cache, Count, Axis, V);
         Lemma_Add_Factor
           (Product_Before => Real_Product_Polynomial (Cache, Count - 1, Warp),
            Sum_Before     => Real_Sum_Polynomial (Cache, Count - 1, Warp, Axis),
            Factor         => Real_Factor_Polynomial (Cache, Count, Warp),
            Term           => Real_Residue_Polynomial (Cache, Count, Warp, Axis),
            V              => V,
            S              => Real_Sum (Cache, Count - 1, Axis, U),
            T              => Real_Term (Cache, Count, Axis, U),
            Total          => Real_Sum (Cache, Count, Axis, U));
         pragma
           Assert
             (Eval (Real_Sum_Polynomial (Cache, Count, Warp, Axis), V)
              = Eval (Real_Product_Polynomial (Cache, Count, Warp), V) * Real_Sum (Cache, Count, Axis, U));
      end if;
   end Lemma_Real_Sum_Polynomial;

   ---------------------------------------------------------------------------
   --  The terms of the pairs over their factors
   ---------------------------------------------------------------------------

   function Pair_Slope_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; Axis : Axis_Name)
      return Polynomial
   is (Difference
         (Scaled
            (R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis)),
             Pair_Offset_Polynomial (Cache, Pair, Warp)),
          Scaled
            (R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis)),
             Pair_Height_Polynomial (Cache, Pair, Warp))))
   with
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post => Is_Polynomial (Pair_Slope_Polynomial'Result) and then Pair_Slope_Polynomial'Result'Last = 1;
   --  R·(V - A·h(V)) - I·B·h(V).

   procedure Lemma_Pair_Slope_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; Axis : Axis_Name; V : Real)
   with
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post =>
       Eval (Pair_Slope_Polynomial (Cache, Pair, Warp, Axis), V)
       = R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis))
         * Eval (Pair_Offset_Polynomial (Cache, Pair, Warp), V)
         - R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis))
           * Eval (Pair_Height_Polynomial (Cache, Pair, Warp), V);

   procedure Lemma_Pair_Slope_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; Axis : Axis_Name; V : Real)
   is
      Residue_Real      : constant Real :=
        R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis));
      Residue_Imaginary : constant Real :=
        R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis));
   begin
      Lemma_Eval_Scaled (Residue_Real, Pair_Offset_Polynomial (Cache, Pair, Warp), V);
      Lemma_Eval_Scaled (Residue_Imaginary, Pair_Height_Polynomial (Cache, Pair, Warp), V);
      Lemma_Eval_Difference
        (Scaled (Residue_Real, Pair_Offset_Polynomial (Cache, Pair, Warp)),
         Scaled (Residue_Imaginary, Pair_Height_Polynomial (Cache, Pair, Warp)),
         V);
   end Lemma_Pair_Slope_Polynomial;

   function Pair_Residue_Polynomial
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; Axis : Axis_Name)
      return Polynomial
   is (if Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp)) = 0.0
       then Constant_Polynomial (0.0)
       else
         Scaled
           (2.0 / (Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp))),
            Product (Pair_Slope_Polynomial (Cache, Pair, Warp, Axis), Scale_Polynomial (Warp))))
   with
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post => Is_Polynomial (Pair_Residue_Polynomial'Result) and then Pair_Residue_Polynomial'Result'Last <= 2;
   --  2·(R·(V - A·h) - I·B·h)·h/(|π|²·W²): the term of one pair times that pair's factor.

   procedure Lemma_Pair_Residue_Transfer (M, Lift, Slope, Scale, Offset, Height, Rr, Ri, L1, L2, H, D : Real)
   with
     Pre  =>
       D /= 0.0
       and then M = (2.0 / D) * Lift
       and then Lift = Slope * Scale
       and then Slope = Rr * Offset - Ri * Height
       and then Offset = L1
       and then Height = L2
       and then Scale = H,
     Post => M * D = 2.0 * ((Rr * L1 - Ri * L2) * H);

   procedure Lemma_Pair_Residue_Transfer (M, Lift, Slope, Scale, Offset, Height, Rr, Ri, L1, L2, H, D : Real) is
   begin
      Lemma_Scaled_By_Quotient (M, 2.0, Lift, D);
   end Lemma_Pair_Residue_Transfer;

   procedure Lemma_Pair_Residue_Value
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; Axis : Axis_Name; V : Real)
   with
     Pre  =>
       Has_Layout (Cache)
       and then Pair <= Cache.Pair_Count
       and then Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp)) /= 0.0,
     Post =>
       Eval (Pair_Residue_Polynomial (Cache, Pair, Warp, Axis), V)
       * (Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp)))
       = 2.0
         * ((R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis))
             * (V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * Model.Warp_Scale (Warp, V))
             - R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis))
               * (R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component)
                  * Model.Warp_Scale (Warp, V)))
            * Model.Warp_Scale (Warp, V));
   --  The value of that polynomial, multiplied through.

   procedure Lemma_Pair_Residue_Value
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; Axis : Axis_Name; V : Real)
   is
      D : constant Real := Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp));
      H : constant Real := Model.Warp_Scale (Warp, V);
   begin
      Lemma_Scale_Polynomial (Warp, V);
      Lemma_Pair_Offset_Polynomial (Cache, Pair, Warp, V);
      Lemma_Pair_Height_Polynomial (Cache, Pair, Warp, V);
      Lemma_Pair_Slope_Polynomial (Cache, Pair, Warp, Axis, V);
      Lemma_Eval_Product (Pair_Slope_Polynomial (Cache, Pair, Warp, Axis), Scale_Polynomial (Warp), V);
      Lemma_Eval_Scaled
        (2.0 / D, Product (Pair_Slope_Polynomial (Cache, Pair, Warp, Axis), Scale_Polynomial (Warp)), V);
      Lemma_Pair_Residue_Transfer
        (M      => Eval (Pair_Residue_Polynomial (Cache, Pair, Warp, Axis), V),
         Lift   => Eval (Product (Pair_Slope_Polynomial (Cache, Pair, Warp, Axis), Scale_Polynomial (Warp)), V),
         Slope  => Eval (Pair_Slope_Polynomial (Cache, Pair, Warp, Axis), V),
         Scale  => Eval (Scale_Polynomial (Warp), V),
         Offset => Eval (Pair_Offset_Polynomial (Cache, Pair, Warp), V),
         Height => Eval (Pair_Height_Polynomial (Cache, Pair, Warp), V),
         Rr     => R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis)),
         Ri     => R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis)),
         L1     => V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * H,
         L2     => R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) * H,
         H      => H,
         D      => D);
   end Lemma_Pair_Residue_Value;

   procedure Lemma_Pair_Term_Unfolded
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Axis : Axis_Name; U : Real)
   with
     Pre  =>
       Has_Layout (Cache) and then Pair <= Cache.Pair_Count and then Pair_Distance_Square (Cache, Pair, U) /= 0.0,
     Post =>
       Pair_Term (Cache, Pair, Axis, U) * Pair_Distance_Square (Cache, Pair, U)
       = 2.0
         * (R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis))
            * (U - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component))
            - R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis))
              * R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component));
   --  The term of a pair away from the pair, multiplied through.

   procedure Lemma_Pair_Term_Unfolded
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Axis : Axis_Name; U : Real) is
   begin
      Lemma_Is_Quotient
        (Pair_Term (Cache, Pair, Axis, U),
         2.0
         * (R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis))
            * (U - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component))
            - R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis))
              * R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component)),
         Pair_Distance_Square (Cache, Pair, U));
   end Lemma_Pair_Term_Unfolded;

   procedure Lemma_Squares_Scale (X, B, H : Real)
   with Post => (X * H) * (X * H) + (B * H) * (B * H) = (X * X + B * B) * (H * H);

   procedure Lemma_Squares_Scale (X, B, H : Real) is
   begin
      pragma Assert ((X * H) * (X * H) = (X * X) * (H * H));
      pragma Assert ((B * H) * (B * H) = (B * B) * (H * H));
   end Lemma_Squares_Scale;

   procedure Lemma_Slope_Scales (Rr, Ri, X, B, H : Real)
   with Post => (Rr * (X * H) - Ri * (B * H)) * H = (Rr * X - Ri * B) * (H * H);

   procedure Lemma_Slope_Scales (Rr, Ri, X, B, H : Real) is
   begin
      pragma Assert (Rr * (X * H) = (Rr * X) * H);
      pragma Assert (Ri * (B * H) = (Ri * B) * H);
      pragma Assert (Rr * (X * H) - Ri * (B * H) = (Rr * X - Ri * B) * H);
      pragma Assert (((Rr * X - Ri * B) * H) * H = (Rr * X - Ri * B) * (H * H));
   end Lemma_Slope_Scales;

   procedure Lemma_Pair_Term_Times_Factor (K, S, HH, D, T, Q, M : Real)
   with
     Pre  => D /= 0.0 and then T * S = 2.0 * K and then Q * D = S * HH and then M * D = 2.0 * (K * HH),
     Post => M = T * Q;
   --  With T = 2·K/S the term and Q = S·h²/D the factor, T·Q·D = 2·K·h².

   procedure Lemma_Pair_Term_Times_Factor (K, S, HH, D, T, Q, M : Real) is
   begin
      pragma Assert ((T * Q) * D = T * (Q * D));
      pragma Assert (T * (S * HH) = (T * S) * HH);
      pragma Assert ((2.0 * K) * HH = 2.0 * (K * HH));
      Lemma_Cancel (M, T * Q, D);
   end Lemma_Pair_Term_Times_Factor;

   procedure Lemma_Pair_Algebra_Named (L1, L2, X, B, Rr, Ri, H, D, S, T, Q, M : Real)
   with
     Pre  =>
       D /= 0.0
       and then L1 = X * H
       and then L2 = B * H
       and then S = X * X + B * B
       and then T * S = 2.0 * (Rr * X - Ri * B)
       and then Q * D = L1 * L1 + L2 * L2
       and then M * D = 2.0 * ((Rr * L1 - Ri * L2) * H),
     Post => M = T * Q;
   --  With X = U - A: the factor's numerator is |U - π|²·h² and the polynomial's is (R·X - I·B)·h².

   procedure Lemma_Pair_Algebra_Named (L1, L2, X, B, Rr, Ri, H, D, S, T, Q, M : Real) is
   begin
      Lemma_Squares_Scale (X, B, H);
      pragma Assert (Q * D = S * (H * H));
      Lemma_Slope_Scales (Rr, Ri, X, B, H);
      pragma Assert (M * D = 2.0 * ((Rr * X - Ri * B) * (H * H)));
      Lemma_Pair_Term_Times_Factor (K => Rr * X - Ri * B, S => S, HH => H * H, D => D, T => T, Q => Q, M => M);
   end Lemma_Pair_Algebra_Named;

   procedure Lemma_Pair_Algebra (A, B, Rr, Ri, H, V, U, D, S, T, Q, M : Real)
   with
     Pre  =>
       U * H = V
       and then D /= 0.0
       and then S = (U - A) * (U - A) + B * B
       and then T * S = 2.0 * (Rr * (U - A) - Ri * B)
       and then Q * D = (V - A * H) * (V - A * H) + (B * H) * (B * H)
       and then M * D = 2.0 * ((Rr * (V - A * H) - Ri * (B * H)) * H),
     Post => M = T * Q;
   --  With T the term of a pair, Q its factor and M the value of its polynomial: V - A·h = (U - A)·h.

   procedure Lemma_Pair_Algebra (A, B, Rr, Ri, H, V, U, D, S, T, Q, M : Real) is
   begin
      pragma Assert (V - A * H = (U - A) * H);
      Lemma_Pair_Algebra_Named
        (L1 => V - A * H,
         L2 => B * H,
         X  => U - A,
         B  => B,
         Rr => Rr,
         Ri => Ri,
         H  => H,
         D  => D,
         S  => S,
         T  => T,
         Q  => Q,
         M  => M);
   end Lemma_Pair_Algebra;

   procedure Lemma_Pair_Residue_Polynomial
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Axis : Axis_Name; V : Real)
   with
     Pre  => Is_Regular (Warp, Cache) and then Pair <= Cache.Pair_Count and then 0.0 <= V and then V <= 1.0,
     Post =>
       Eval (Pair_Residue_Polynomial (Cache, Pair, Warp, Axis), V)
       = Pair_Term (Cache, Pair, Axis, Model.Distance (Warp, V)) * Model.Pair_Factor (Cache, Pair, Warp, V);

   procedure Lemma_Pair_Residue_Polynomial
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Axis : Axis_Name; V : Real)
   is
      U : constant Real := Model.Distance (Warp, V);
   begin
      Lemma_Distance_Times_Scale (Warp, V);
      Lemma_Pair_Pole_Is_Off (Warp, Cache, Pair, U);
      Lemma_Pair_Term_Unfolded (Cache, Pair, Axis, U);
      Lemma_Pair_Factor_Unfolded (Cache, Pair, Warp, V);
      Lemma_Pair_Residue_Value (Cache, Pair, Warp, Axis, V);
      Lemma_Pair_Algebra
        (A  => R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component),
         B  => R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component),
         Rr => R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis)),
         Ri => R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis)),
         H  => Model.Warp_Scale (Warp, V),
         V  => V,
         U  => U,
         D  => Model.Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp)),
         S  => Pair_Distance_Square (Cache, Pair, U),
         T  => Pair_Term (Cache, Pair, Axis, U),
         Q  => Model.Pair_Factor (Cache, Pair, Warp, V),
         M  => Eval (Pair_Residue_Polynomial (Cache, Pair, Warp, Axis), V));
   end Lemma_Pair_Residue_Polynomial;

   function Pair_Sum_Polynomial
     (Cache : Rational_Antiderivative; Count : Natural; Warp : Dimensionless; Axis : Axis_Name) return Polynomial
   is (if Count = 0
       then Constant_Polynomial (0.0)
       else
         Sum
           (Product (Pair_Sum_Polynomial (Cache, Count - 1, Warp, Axis), Pair_Factor_Polynomial (Cache, Count, Warp)),
            Product
              (Pair_Residue_Polynomial (Cache, Count, Warp, Axis), Pair_Product_Polynomial (Cache, Count - 1, Warp))))
   with
     Pre                => Has_Layout (Cache) and then Count <= Cache.Pair_Count,
     Post               =>
       Is_Polynomial (Pair_Sum_Polynomial'Result) and then Pair_Sum_Polynomial'Result'Last <= 2 * Count,
     Subprogram_Variant => (Decreases => Count);
   --  The product of the factors of the first Count pairs, times the sum of the terms of those pairs.

   procedure Lemma_Pair_Sum_Polynomial
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Count : Natural; Axis : Axis_Name; V : Real)
   with
     Pre                =>
       Is_Regular (Warp, Cache) and then Count <= Cache.Pair_Count and then 0.0 <= V and then V <= 1.0,
     Post               =>
       Eval (Pair_Sum_Polynomial (Cache, Count, Warp, Axis), V)
       = Eval (Pair_Product_Polynomial (Cache, Count, Warp), V)
         * Pair_Sum (Cache, Count, Axis, Model.Distance (Warp, V)),
     Subprogram_Variant => (Decreases => Count);

   procedure Lemma_Pair_Sum_Polynomial
     (Warp : Dimensionless; Cache : Rational_Antiderivative; Count : Natural; Axis : Axis_Name; V : Real)
   is
      U : constant Real := Model.Distance (Warp, V);
   begin
      if Count = 0 then
         Lemma_Eval_Constant (0.0, V);
         Lemma_Zero_Product
           (X => Eval (Pair_Sum_Polynomial (Cache, Count, Warp, Axis), V),
            Q => Eval (Pair_Product_Polynomial (Cache, Count, Warp), V),
            S => Pair_Sum (Cache, Count, Axis, U));
      else
         Lemma_Pair_Sum_Polynomial (Warp, Cache, Count - 1, Axis, V);
         Lemma_Pair_Factor_Polynomial (Cache, Count, Warp, V);
         Lemma_Pair_Residue_Polynomial (Warp, Cache, Count, Axis, V);
         Lemma_Add_Factor
           (Product_Before => Pair_Product_Polynomial (Cache, Count - 1, Warp),
            Sum_Before     => Pair_Sum_Polynomial (Cache, Count - 1, Warp, Axis),
            Factor         => Pair_Factor_Polynomial (Cache, Count, Warp),
            Term           => Pair_Residue_Polynomial (Cache, Count, Warp, Axis),
            V              => V,
            S              => Pair_Sum (Cache, Count - 1, Axis, U),
            T              => Pair_Term (Cache, Count, Axis, U),
            Total          => Pair_Sum (Cache, Count, Axis, U));
         pragma
           Assert
             (Eval (Pair_Sum_Polynomial (Cache, Count, Warp, Axis), V)
              = Eval (Pair_Product_Polynomial (Cache, Count, Warp), V) * Pair_Sum (Cache, Count, Axis, U));
      end if;
   end Lemma_Pair_Sum_Polynomial;

   ---------------------------------------------------------------------------
   --  The retained tangent times Q
   ---------------------------------------------------------------------------

   function Unscaled_Retained_Polynomial
     (Cache : Rational_Antiderivative; Warp : Dimensionless; Axis : Axis_Name) return Polynomial
   is (Sum
         (Sum
            (Scaled (R (Cache.Constant_Tangent (Axis)), Pole_Product_Polynomial (Cache, Warp)),
             Product
               (Real_Sum_Polynomial (Cache, Cache.Real_Pole_Count, Warp, Axis),
                Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp))),
          Product
            (Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp),
             Pair_Sum_Polynomial (Cache, Cache.Pair_Count, Warp, Axis))))
   with
     Pre  => Has_Layout (Cache),
     Post =>
       Is_Polynomial (Unscaled_Retained_Polynomial'Result)
       and then Unscaled_Retained_Polynomial'Result'Last <= Maximum_Rational_Degree;
   --  The retained tangent times the factors of every pole: the constant times all of them, the real poles'
   --  sum times the pairs' factors, and the pairs' sum times the real poles' factors.

   function Retained_Polynomial
     (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless; Axis : Axis_Name) return Polynomial
   is (Scaled (R (Denominator_Scale), Unscaled_Retained_Polynomial (Cache, Warp, Axis)))
   with
     Pre  => Has_Layout (Cache),
     Post =>
       Is_Polynomial (Retained_Polynomial'Result) and then Retained_Polynomial'Result'Last <= Maximum_Rational_Degree;
   --  Q·R(U(V)), which has no denominator left.

   procedure Lemma_Retained_Transfer (X, A1, A2, A3, Poles, C, Rq, Pq, Rm, Pm : Real)
   with
     Pre  =>
       X = (A1 + A2) + A3 and then A1 = C * Poles and then Poles = Rq * Pq and then A2 = Rm * Pq and then A3 = Rq * Pm,
     Post => X = (C * (Rq * Pq) + Rm * Pq) + Rq * Pm;

   procedure Lemma_Retained_Transfer (X, A1, A2, A3, Poles, C, Rq, Pq, Rm, Pm : Real) is null;

   procedure Lemma_Unscaled_Retained_Value
     (Cache : Rational_Antiderivative; Warp : Dimensionless; Axis : Axis_Name; V : Real)
   with
     Pre  => Has_Layout (Cache),
     Post =>
       Eval (Unscaled_Retained_Polynomial (Cache, Warp, Axis), V)
       = (R (Cache.Constant_Tangent (Axis))
          * (Eval (Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp), V)
             * Eval (Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp), V))
          + Eval (Real_Sum_Polynomial (Cache, Cache.Real_Pole_Count, Warp, Axis), V)
            * Eval (Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp), V))
         + Eval (Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp), V)
           * Eval (Pair_Sum_Polynomial (Cache, Cache.Pair_Count, Warp, Axis), V);
   --  The value of that polynomial from the values of its four parts.

   procedure Lemma_Unscaled_Retained_Value
     (Cache : Rational_Antiderivative; Warp : Dimensionless; Axis : Axis_Name; V : Real)
   is
      C : constant Real := R (Cache.Constant_Tangent (Axis));
   begin
      Lemma_Eval_Product
        (Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp),
         Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp),
         V);
      Lemma_Eval_Scaled (C, Pole_Product_Polynomial (Cache, Warp), V);
      Lemma_Eval_Product
        (Real_Sum_Polynomial (Cache, Cache.Real_Pole_Count, Warp, Axis),
         Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp),
         V);
      Lemma_Eval_Product
        (Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp),
         Pair_Sum_Polynomial (Cache, Cache.Pair_Count, Warp, Axis),
         V);
      Lemma_Eval_Sum
        (Scaled (C, Pole_Product_Polynomial (Cache, Warp)),
         Product
           (Real_Sum_Polynomial (Cache, Cache.Real_Pole_Count, Warp, Axis),
            Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp)),
         V);
      Lemma_Eval_Sum
        (Sum
           (Scaled (C, Pole_Product_Polynomial (Cache, Warp)),
            Product
              (Real_Sum_Polynomial (Cache, Cache.Real_Pole_Count, Warp, Axis),
               Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp))),
         Product
           (Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp),
            Pair_Sum_Polynomial (Cache, Cache.Pair_Count, Warp, Axis)),
         V);
      Lemma_Retained_Transfer
        (X     => Eval (Unscaled_Retained_Polynomial (Cache, Warp, Axis), V),
         A1    => Eval (Scaled (C, Pole_Product_Polynomial (Cache, Warp)), V),
         A2    =>
           Eval
             (Product
                (Real_Sum_Polynomial (Cache, Cache.Real_Pole_Count, Warp, Axis),
                 Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp)),
              V),
         A3    =>
           Eval
             (Product
                (Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp),
                 Pair_Sum_Polynomial (Cache, Cache.Pair_Count, Warp, Axis)),
              V),
         Poles => Eval (Pole_Product_Polynomial (Cache, Warp), V),
         C     => C,
         Rq    => Eval (Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp), V),
         Pq    => Eval (Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp), V),
         Rm    => Eval (Real_Sum_Polynomial (Cache, Cache.Real_Pole_Count, Warp, Axis), V),
         Pm    => Eval (Pair_Sum_Polynomial (Cache, Cache.Pair_Count, Warp, Axis), V));
   end Lemma_Unscaled_Retained_Value;

   procedure Lemma_Combine (X, Y, S, C, Rq, Pq, Rm, Pm, Rs, Ps, Qv, Rv : Real)
   with
     Pre  =>
       X = S * Y
       and then Y = (C * (Rq * Pq) + Rm * Pq) + Rq * Pm
       and then Rm = Rq * Rs
       and then Pm = Pq * Ps
       and then Qv = S * Rq * Pq
       and then Rv = C + Rs + Ps,
     Post => X = Qv * Rv;
   --  s·(c·Πr·Πp + (Πr·Σr)·Πp + Πr·(Πp·Σp)) = (s·Πr·Πp)·(c + Σr + Σp).

   procedure Lemma_Combine (X, Y, S, C, Rq, Pq, Rm, Pm, Rs, Ps, Qv, Rv : Real) is
      Poles : constant Real := Rq * Pq;
   begin
      pragma Assert (Rm * Pq = Poles * Rs);
      pragma Assert (Rq * Pm = Poles * Ps);
      pragma Assert ((C * Poles + Poles * Rs) + Poles * Ps = Poles * Rv);
      pragma Assert (Y = Poles * Rv);
      pragma Assert (S * (Poles * Rv) = (S * Poles) * Rv);
      pragma Assert (S * Poles = Qv);
   end Lemma_Combine;

   procedure Lemma_Retained_Polynomial
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Axis              : Axis_Name;
      V                 : Real)
   with
     Pre  => Is_Regular (Warp, Cache) and then 0.0 <= V and then V <= 1.0,
     Post =>
       Eval (Retained_Polynomial (Cache, Denominator_Scale, Warp, Axis), V)
       = Model.Cached_Denominator (Cache, Denominator_Scale, Warp, V) * Value (Cache, Axis, Model.Distance (Warp, V));

   procedure Lemma_Retained_Polynomial
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Axis              : Axis_Name;
      V                 : Real)
   is
      U : constant Real := Model.Distance (Warp, V);
   begin
      Lemma_Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp, V);
      Lemma_Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp, V);
      Lemma_Real_Sum_Polynomial (Warp, Cache, Cache.Real_Pole_Count, Axis, V);
      Lemma_Pair_Sum_Polynomial (Warp, Cache, Cache.Pair_Count, Axis, V);
      Lemma_Unscaled_Retained_Value (Cache, Warp, Axis, V);
      Lemma_Eval_Scaled (R (Denominator_Scale), Unscaled_Retained_Polynomial (Cache, Warp, Axis), V);
      Lemma_Combine
        (X  => Eval (Retained_Polynomial (Cache, Denominator_Scale, Warp, Axis), V),
         Y  => Eval (Unscaled_Retained_Polynomial (Cache, Warp, Axis), V),
         S  => R (Denominator_Scale),
         C  => R (Cache.Constant_Tangent (Axis)),
         Rq => Eval (Real_Product_Polynomial (Cache, Cache.Real_Pole_Count, Warp), V),
         Pq => Eval (Pair_Product_Polynomial (Cache, Cache.Pair_Count, Warp), V),
         Rm => Eval (Real_Sum_Polynomial (Cache, Cache.Real_Pole_Count, Warp, Axis), V),
         Pm => Eval (Pair_Sum_Polynomial (Cache, Cache.Pair_Count, Warp, Axis), V),
         Rs => Real_Sum (Cache, Cache.Real_Pole_Count, Axis, U),
         Ps => Pair_Sum (Cache, Cache.Pair_Count, Axis, U),
         Qv => Model.Cached_Denominator (Cache, Denominator_Scale, Warp, V),
         Rv => Value (Cache, Axis, U));
   end Lemma_Retained_Polynomial;

   ---------------------------------------------------------------------------
   --  The numerator error g
   ---------------------------------------------------------------------------

   function Numerator_Error_Polynomial
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Axis              : Axis_Name) return Polynomials.Polynomial
   is (Difference
         (Product
            (Cached_Denominator_Polynomial (Cache, Denominator_Scale, Warp),
             Tangent_Numerator_Polynomial (Frame, Coefficients, Axis)),
          Product
            (Ideal_Denominator_Polynomial (Coefficients),
             Retained_Polynomial (Cache, Denominator_Scale, Warp, Axis))));

   procedure Lemma_Factor_Out (X, A1, A2, Qe, Pe, De, Me, Q, P, D, Rv, G : Real)
   with
     Pre  =>
       X = A1 - A2
       and then A1 = Qe * Pe
       and then A2 = De * Me
       and then Qe = Q
       and then Pe = P
       and then De = D
       and then Me = Q * Rv
       and then G = Q * (P - D * Rv),
     Post => X = G;
   --  Q·P - D·(Q·R) = Q·(P - D·R).

   procedure Lemma_Factor_Out (X, A1, A2, Qe, Pe, De, Me, Q, P, D, Rv, G : Real) is
   begin
      pragma Assert (D * (Q * Rv) = Q * (D * Rv));
      pragma Assert (Q * P - Q * (D * Rv) = Q * (P - D * Rv));
   end Lemma_Factor_Out;

   procedure Lemma_Numerator_Error
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Axis              : Axis_Name;
      V                 : Real) is
   begin
      Lemma_Cached_Denominator_Polynomial (Cache, Denominator_Scale, Warp, V);
      Lemma_Tangent_Numerator_Polynomial (Frame, Coefficients, Axis, V);
      Lemma_Ideal_Denominator_Polynomial (Coefficients, V);
      Lemma_Retained_Polynomial (Warp, Cache, Denominator_Scale, Axis, V);
      Lemma_Eval_Product
        (Cached_Denominator_Polynomial (Cache, Denominator_Scale, Warp),
         Tangent_Numerator_Polynomial (Frame, Coefficients, Axis),
         V);
      Lemma_Eval_Product
        (Ideal_Denominator_Polynomial (Coefficients), Retained_Polynomial (Cache, Denominator_Scale, Warp, Axis), V);
      Lemma_Eval_Difference
        (Product
           (Cached_Denominator_Polynomial (Cache, Denominator_Scale, Warp),
            Tangent_Numerator_Polynomial (Frame, Coefficients, Axis)),
         Product
           (Ideal_Denominator_Polynomial (Coefficients), Retained_Polynomial (Cache, Denominator_Scale, Warp, Axis)),
         V);
      Lemma_Factor_Out
        (X  => Eval (Numerator_Error_Polynomial (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis), V),
         A1 =>
           Eval
             (Product
                (Cached_Denominator_Polynomial (Cache, Denominator_Scale, Warp),
                 Tangent_Numerator_Polynomial (Frame, Coefficients, Axis)),
              V),
         A2 =>
           Eval
             (Product
                (Ideal_Denominator_Polynomial (Coefficients),
                 Retained_Polynomial (Cache, Denominator_Scale, Warp, Axis)),
              V),
         Qe => Eval (Cached_Denominator_Polynomial (Cache, Denominator_Scale, Warp), V),
         Pe => Eval (Tangent_Numerator_Polynomial (Frame, Coefficients, Axis), V),
         De => Eval (Ideal_Denominator_Polynomial (Coefficients), V),
         Me => Eval (Retained_Polynomial (Cache, Denominator_Scale, Warp, Axis), V),
         Q  => Model.Cached_Denominator (Cache, Denominator_Scale, Warp, V),
         P  => Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, V),
         D  => Denominator (Coefficients, V),
         Rv => Value (Cache, Axis, Model.Distance (Warp, V)),
         G  => Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, V));
   end Lemma_Numerator_Error;

   ---------------------------------------------------------------------------
   --  The numerator error and the tangent error
   ---------------------------------------------------------------------------

   procedure Lemma_Divide_Out (G, E, Q, P, D, Rv : Real)
   with Pre => D >= 1.0 and then G = Q * (P - D * Rv) and then E = P / D - Rv, Post => G = Q * D * E;
   --  Q·(P - D·R) = Q·D·(P/D - R) wherever D is not zero.

   procedure Lemma_Divide_Out (G, E, Q, P, D, Rv : Real) is
   begin
      Lemma_Quotient_Times_Divisor (P, D);
      pragma Assert (D * (P / D - Rv) = P - D * Rv);
      pragma Assert (Q * D * (P / D - Rv) = Q * (D * (P / D - Rv)));
   end Lemma_Divide_Out;

   procedure Lemma_Tangent_Error
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Axis              : Axis_Name;
      V                 : Real) is
   begin
      Lemma_Divide_Out
        (G  => Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, V),
         E  => Model.Tangent_Error (Frame, Coefficients, Warp, Cache, Axis, V),
         Q  => Model.Cached_Denominator (Cache, Denominator_Scale, Warp, V),
         P  => Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, V),
         D  => Denominator (Coefficients, V),
         Rv => Value (Cache, Axis, Model.Distance (Warp, V)));
   end Lemma_Tangent_Error;

   ---------------------------------------------------------------------------
   --  An axis on which both tangents vanish
   ---------------------------------------------------------------------------

   procedure Lemma_Chart_Vanishes (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; V : Real)
   with
     Pre  => (for all K in Chart_Coefficient_Index => Coefficients (K, Component) = 0.0),
     Post => Chart_Value (Coefficients, Component, V) = 0.0;
   --  A chart coordinate whose coefficients are all zero is zero everywhere, not only on the curve.

   procedure Lemma_Chart_Vanishes (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; V : Real) is
   begin
      pragma Assert (for all I in Chart_Coefficient_Index => Chart_Controls (Coefficients, Component) (I) = 0.0);
      Bernstein.Binomial_Form.Lemma_Value_Of_Constant (Chart_Controls (Coefficients, Component), 0.0, V);
   end Lemma_Chart_Vanishes;

   procedure Lemma_Frame_Term_Vanishes
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Axis         : Axis_Name;
      Component    : Chart_Component_Index;
      V            : Real)
   with
     Pre  =>
       Frame (Component) (Axis) = 0.0
       or else (for all K in Chart_Coefficient_Index => Coefficients (K, Component) = 0.0),
     Post => Numerator (Coefficients, Component, V) * R (Frame (Component) (Axis)) = 0.0;
   --  A frame vector contributes nothing on an axis where it has no part, or if its chart coordinate is zero.

   procedure Lemma_Frame_Term_Vanishes
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Axis         : Axis_Name;
      Component    : Chart_Component_Index;
      V            : Real) is
   begin
      if Frame (Component) (Axis) = 0.0 then
         Lemma_Zero (Frame (Component) (Axis));
      else
         Lemma_Chart_Vanishes (Coefficients, Component, V);
         pragma Assert (Numerator (Coefficients, Component, V) = 0.0);
      end if;
   end Lemma_Frame_Term_Vanishes;

   procedure Lemma_Real_Sum_Vanishes (Cache : Rational_Antiderivative; Count : Natural; Axis : Axis_Name; U : Real)
   with
     Pre                =>
       Count <= Cache.Real_Pole_Count
       and then (for all Slot in 1 .. Count => Cache.Pole_Slots (Slot).Residue_Component (Axis) = 0.0),
     Post               => Real_Sum (Cache, Count, Axis, U) = 0.0,
     Subprogram_Variant => (Decreases => Count);
   --  Real poles whose residues are zero contribute nothing, wherever U is.

   procedure Lemma_Real_Sum_Vanishes (Cache : Rational_Antiderivative; Count : Natural; Axis : Axis_Name; U : Real) is
   begin
      if Count > 0 then
         Lemma_Real_Sum_Vanishes (Cache, Count - 1, Axis, U);
         Lemma_Zero (Cache.Pole_Slots (Count).Residue_Component (Axis));
         pragma Assert (Real_Term (Cache, Count, Axis, U) = 0.0);
      end if;
   end Lemma_Real_Sum_Vanishes;

   procedure Lemma_Pair_Sum_Vanishes (Cache : Rational_Antiderivative; Count : Natural; Axis : Axis_Name; U : Real)
   with
     Pre                =>
       Has_Layout (Cache)
       and then Count <= Cache.Pair_Count
       and then
         (for all Slot in Rational_Degree_Slot =>
            (if Slot <= Cache.Real_Pole_Count + 2 * Count
             then Cache.Pole_Slots (Slot).Residue_Component (Axis) = 0.0)),
     Post               => Pair_Sum (Cache, Count, Axis, U) = 0.0,
     Subprogram_Variant => (Decreases => Count);
   --  Likewise for pairs.

   procedure Lemma_Pair_Sum_Vanishes (Cache : Rational_Antiderivative; Count : Natural; Axis : Axis_Name; U : Real) is
   begin
      if Count > 0 then
         Lemma_Pair_Sum_Vanishes (Cache, Count - 1, Axis, U);
         Lemma_Zero (Cache.Pole_Slots (Pair_Real_Slot (Cache, Count)).Residue_Component (Axis));
         Lemma_Zero (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Count)).Residue_Component (Axis));
         pragma Assert (Pair_Term (Cache, Count, Axis, U) = 0.0);
      end if;
   end Lemma_Pair_Sum_Vanishes;

   procedure Lemma_Inactive_Axis
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Warp         : Dimensionless;
      Cache        : Rational_Antiderivative;
      Axis         : Axis_Name;
      V            : Real)
   is
      U : constant Real := Model.Distance (Warp, V);
   begin
      --  The ideal numerator: every frame vector has no part on the axis, or its chart coordinate is zero.
      Lemma_Zero (Frame (0) (Axis));
      Lemma_Frame_Term_Vanishes (Frame, Coefficients, Axis, 1, V);
      Lemma_Frame_Term_Vanishes (Frame, Coefficients, Axis, 2, V);
      Lemma_Frame_Term_Vanishes (Frame, Coefficients, Axis, 3, V);
      pragma Assert (Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, V) = 0.0);
      pragma Assert (Model.Ideal_Tangent (Frame, Coefficients, Axis, V) = 0.0);

      --  The retained tangent: its constant and every residue in use are zero.
      Lemma_Zero (Cache.Constant_Tangent (Axis));
      Lemma_Real_Sum_Vanishes (Cache, Cache.Real_Pole_Count, Axis, U);
      Lemma_Pair_Sum_Vanishes (Cache, Cache.Pair_Count, Axis, U);
      pragma Assert (Value (Cache, Axis, U) = 0.0);
   end Lemma_Inactive_Axis;

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Structure;
