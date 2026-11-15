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
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

with Prunt.Motion_Planner.Stereographic_Kernel.Elementary; use Prunt.Motion_Planner.Stereographic_Kernel.Elementary;
with Prunt.Motion_Planner.Stereographic_Kernel.Intervals;  use Prunt.Motion_Planner.Stereographic_Kernel.Intervals;

package body Prunt.Motion_Planner.Stereographic_Kernel.Square_Roots
  with SPARK_Mode => On
is
   Half_Last_Real : constant := Dimensionless'Last / 2.0;

   Large : constant := 2.0**1022;
   --  Below Half_Last. Its square exceeds every floating-point number.

   Guard_Factor : constant := 2.0**1023;
   --  Above the quotient of a value which passed the guard by a candidate below one.

   ---------------------------------------------------------------------------
   --  Floating-point facts
   ---------------------------------------------------------------------------

   procedure Lemma_Smallest_Positive (X : Dimensionless)
   with Ghost, Pre => X > 0.0, Post => R (X) >= Twice_Eta;
   --  No positive floating-point number is below the smallest subnormal one.

   procedure Lemma_Smallest_Positive (X : Dimensionless) is null;

   procedure Lemma_Guard_Product (X : Dimensionless)
   with
     Ghost,
     Pre  => X >= 0.0 and then X <= 1.0,
     Post =>
       abs (R ((Dimensionless'Last / 2.0) * X) - Half_Last_Real * R (X)) <= Epsilon * Half_Last_Real * R (X) + Eta;
   --  The product in the guard is one rounded multiplication, which cannot overflow.

   procedure Lemma_Guard_Product (X : Dimensionless) is null;

   procedure Lemma_Quotient_Is_Rounded (A, B : Dimensionless)
   with
     Ghost,
     Pre  => A >= 0.0 and then B > 0.0 and then R (A) / R (B) <= R (Dimensionless'Last),
     Post => Is_Rounding_Of (A / B, R (A) / R (B));
   --  A quotient which cannot overflow is one rounded division, whatever the magnitudes of its operands.

   procedure Lemma_Quotient_Is_Rounded (A, B : Dimensionless) is null;

   ---------------------------------------------------------------------------
   --  Real facts
   ---------------------------------------------------------------------------

   procedure Lemma_Ratio_Is_Below (V, X, K : Real)
   with Ghost, Pre => X > 0.0 and then V <= K * X, Post => V / X <= K;

   procedure Lemma_Ratio_Is_Below (V, X, K : Real) is
   begin
      pragma Assert ((V / X) * X = V);
      pragma Assert (if V / X > K then (V / X) * X > K * X);
   end Lemma_Ratio_Is_Below;

   procedure Lemma_Square_Is_Above (V, X : Real)
   with Ghost, Pre => X > 0.0 and then V / X <= X, Post => V <= X * X;

   procedure Lemma_Square_Is_Above (V, X : Real) is
   begin
      pragma Assert ((V / X) * X = V);
      pragma Assert ((V / X) * X <= X * X);
   end Lemma_Square_Is_Above;

   ---------------------------------------------------------------------------
   --  The steps of the search
   ---------------------------------------------------------------------------

   procedure Lemma_Quotient_Is_Bounded (Value, Result : Dimensionless)
   with
     Ghost,
     Pre  =>
       Value > 0.0
       and then Value <= Dimensionless'Last
       and then Result > 0.0
       and then Result < Dimensionless'Last
       and then (if Result < 1.0 then Value <= (Dimensionless'Last / 2.0) * Result),
     Post => R (Value) / R (Result) <= R (Dimensionless'Last);
   --  What the guard leaves cannot overflow when it is divided: a candidate of at least one does not enlarge
   --  Value, and a smaller one is at least Value over half the largest number, give or take a rounding.

   procedure Lemma_Quotient_Is_Bounded (Value, Result : Dimensionless) is
   begin
      if Result < 1.0 then
         Lemma_Guard_Product (Result);
         Lemma_Smallest_Positive (Result);
         pragma Assert (R (Value) <= Half_Last_Real * R (Result) + Epsilon * Half_Last_Real * R (Result) + Eta);
         pragma Assert (Eta <= 0.5 * R (Result));
         pragma Assert (R (Value) <= Guard_Factor * R (Result));
         Lemma_Ratio_Is_Below (R (Value), R (Result), Guard_Factor);
      else
         pragma Assert (R (Result) >= 1.0);
         pragma Assert (R (Value) <= R (Value) * R (Result));
         Lemma_Ratio_Is_Below (R (Value), R (Result), R (Value));
      end if;
   end Lemma_Quotient_Is_Bounded;

   procedure Lemma_Square_Covers (Value, Result, Quotient : Dimensionless)
   with
     Ghost,
     Pre  =>
       Value > 0.0
       and then Value <= Dimensionless'Last
       and then Result > 0.0
       and then Result < Dimensionless'Last
       and then Is_Finite (Quotient)
       and then Is_Rounding_Of (Quotient, R (Value) / R (Result))
       and then Result >= Up (Quotient),
     Post => R (Result) * R (Result) >= R (Value);
   --  A candidate which is no less than the padded quotient of Value by it has a square of at least Value: the
   --  padding takes the rounded quotient above the exact one.

   procedure Lemma_Square_Covers (Value, Result, Quotient : Dimensionless) is
   begin
      if abs Quotient <= Half_Last then
         Lemma_Inflate_Encloses (Quotient, R (Value) / R (Result));
         pragma Assert (R (Value) / R (Result) <= R (Up (Quotient)));
         pragma Assert (R (Value) / R (Result) <= R (Result));
         Lemma_Square_Is_Above (R (Value), R (Result));
      else
         --  So large a quotient is not padded, but the square of anything above it is beyond every number.
         pragma Assert (R (Value) / R (Result) > 0.0);
         pragma Assert (Quotient > Half_Last);
         pragma Assert (Result > Half_Last);
         pragma Assert (R (Result) >= Large);
         pragma Assert (R (Result) * R (Result) >= Large * R (Result));
         pragma Assert (Large * R (Result) >= Large * Large);
         pragma Assert (R (Value) <= R (Dimensionless'Last));
         pragma Assert (R (Dimensionless'Last) <= Large * Large);
      end if;
   end Lemma_Square_Covers;

   ---------------------------------------------------------------------------
   --  The postcondition, one result at a time
   ---------------------------------------------------------------------------
   --
   --  The search leaves by six return statements, and the tool asks for the postcondition once, of whatever any of
   --  them returns. Given as arithmetic that question is too much for the provers; so each return is preceded by a
   --  lemma which states that its own value is a root bound, a predicate which the question then only has to match.

   function Is_Root_Bound (Value, Root : Dimensionless) return Boolean
   with
     Ghost,
     Post =>
       Is_Root_Bound'Result
       = (Root = Dimensionless'Last
          or else
            (Value >= 0.0
             and then Root >= 0.0
             and then Root < Dimensionless'Last
             and then R (Root) * R (Root) >= R (Value)));
   --  What the postcondition says of a result. Given by a postcondition, rather than as an expression function,
   --  to have the tool keep it as one term.

   function Is_Root_Bound (Value, Root : Dimensionless) return Boolean is
   begin
      return
        Root = Dimensionless'Last
        or else
          (Value >= 0.0
           and then Root >= 0.0
           and then Root < Dimensionless'Last
           and then R (Root) * R (Root) >= R (Value));
   end Is_Root_Bound;

   procedure Lemma_Last_Is_Bound (Value : Dimensionless)
   with Ghost, Post => Is_Root_Bound (Value, Dimensionless'Last);
   --  The failure value.

   procedure Lemma_Last_Is_Bound (Value : Dimensionless) is null;

   procedure Lemma_Zero_Is_Bound (Value : Dimensionless)
   with Ghost, Pre => Value = 0.0, Post => Is_Root_Bound (Value, 0.0);
   --  Zero is its own square root, whichever its sign.

   procedure Lemma_Zero_Is_Bound (Value : Dimensionless) is
   begin
      pragma Assert (R (Value) = 0.0);
      pragma Assert (R (0.0) = 0.0);
      pragma Assert (R (0.0) * R (0.0) >= R (Value));
   end Lemma_Zero_Is_Bound;

   procedure Lemma_Root_Is_Bound (Value, Root : Dimensionless)
   with
     Ghost,
     Pre  =>
       Value >= 0.0 and then Root >= 0.0 and then Root < Dimensionless'Last and then R (Root) * R (Root) >= R (Value),
     Post => Is_Root_Bound (Value, Root);
   --  A certified candidate.

   procedure Lemma_Root_Is_Bound (Value, Root : Dimensionless) is null;

   function Certified_Upper_Square_Root (Value : Dimensionless) return Dimensionless
   with
     Refined_Post =>
       (if Value = 0.0 then Certified_Upper_Square_Root'Result = 0.0)
       and then Is_Root_Bound (Value, Certified_Upper_Square_Root'Result)
   is
      Result : Dimensionless;
   begin
      if Value < 0.0 or else not Is_Finite (Value) then
         Lemma_Last_Is_Bound (Value);
         return Dimensionless'Last;
      elsif Value = 0.0 then
         Lemma_Zero_Is_Bound (Value);
         return 0.0;
      end if;

      Result := Up (Dimensionless_Math.Sqrt (Value));
      for Attempt in 1 .. 8 loop
         pragma Loop_Invariant (Value > 0.0);
         if Result <= 0.0 or else not Is_Finite (Result) or else Result >= Dimensionless'Last then
            Lemma_Last_Is_Bound (Value);
            return Dimensionless'Last;
         end if;

         --  The quotient below cannot overflow for the square root of Value, or anything near it. A candidate so
         --  small that it could is no square root, and ends the search as the overflow itself used to.
         if Result < 1.0 and then Value > (Dimensionless'Last / 2.0) * Result then
            Lemma_Last_Is_Bound (Value);
            return Dimensionless'Last;
         end if;

         Lemma_Quotient_Is_Bounded (Value, Result);
         Lemma_Quotient_Is_Rounded (Value, Result);

         declare
            Quotient_Upper : constant Dimensionless := Up (Value / Result);
         begin
            --  Result >= Value / Result is equivalent to Result**2 >= Value for positive Result. Certifying with an
            --  outward-rounded quotient avoids squaring near Model_Small: the generic interval multiplier must use
            --  an absolute Model_Small inflation there, which is intentionally conservative but too wide to prove
            --  this particular inequality.
            if Is_Finite (Quotient_Upper) and then Result >= Quotient_Upper then
               Lemma_Square_Covers (Value, Result, Value / Result);
               Lemma_Root_Is_Bound (Value, Result);
               return Result;
            end if;
         end;
         Result := Up (Result);
      end loop;
      Lemma_Last_Is_Bound (Value);
      return Dimensionless'Last;
   end Certified_Upper_Square_Root;

end Prunt.Motion_Planner.Stereographic_Kernel.Square_Roots;
