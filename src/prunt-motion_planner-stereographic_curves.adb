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
--  The contracts and assertions of this body are for GNATprove. None is executed, whatever the build profile.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Ada.Numerics;
with Ada.Numerics.Generic_Complex_Types;
with Prunt.LAPACK.Eigenvalues;
with Prunt.Motion_Planner.Stereographic_Kernel.Sample_Nodes.Table;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent;
with Prunt.Motion_Planner.Stereographic_Kernel.Square_Roots;

pragma Extensions_Allowed (On);

package body Prunt.Motion_Planner.Stereographic_Curves
  with SPARK_Mode => On
is
   use Stereographic_Kernel.Square_Roots;
   use type Stereographic_Kernel.Certified_Geometry.Certificate_Kind;
   use type Stereographic_Kernel.Certified_Evaluator.Certificate_Kind;

   function Build_Binomial_Table return Binomial_Table_Type is
      Result : Binomial_Table_Type := [others => [others => 0.0]];
   begin
      --  Fill Pascal's triangle once so later Bernstein conversions use exact table lookups:
      --
      --     C(n, 0) = C(n, n) = 1,   C(n, k) = C(n - 1, k - 1) + C(n - 1, k).
      --
      --  No entry reaches Binomial_Limit. Taking the smaller of it and each sum states that for the proof without an
      --  argument about how fast the rows grow.
      for N in 0 .. Maximum_Binomial_Degree loop
         pragma
           Loop_Invariant
             (for all M in Binomial_Table_Type'Range (1) =>
                (for all J in Binomial_Table_Type'Range (2) =>
                   Result (M, J) >= 0.0
                   and then Result (M, J) <= Binomial_Limit
                   and then (if M < N and then J <= M then Result (M, J) >= 1.0)));
         Result (N, 0) := 1.0;
         Result (N, N) := 1.0;
         if N > 1 then
            for K in 1 .. N - 1 loop
               pragma
                 Loop_Invariant
                   (for all M in Binomial_Table_Type'Range (1) =>
                      (for all J in Binomial_Table_Type'Range (2) =>
                         Result (M, J) >= 0.0
                         and then Result (M, J) <= Binomial_Limit
                         and then (if M < N and then J <= M then Result (M, J) >= 1.0)
                         and then (if M = N and then (J < K or else J = N) then Result (M, J) >= 1.0)));
               Result (N, K) := Dimensionless'Min (Binomial_Limit, Result (N - 1, K - 1) + Result (N - 1, K));
            end loop;
         end if;
         pragma Assert (for all J in Binomial_Table_Type'Range (2) => (if J <= N then Result (N, J) >= 1.0));
      end loop;
      return Result;
   end Build_Binomial_Table;

   Binomial_Table : constant Binomial_Table_Type := Build_Binomial_Table;

   function Binomial_Index (N, K : Natural) return Natural
   is ((N + 1)**2 / 4 + Natural'Min (K, N - K))
   with
     Pre  => N <= Maximum_Derivative_Bernstein_Degree and then K <= N,
     Post => Binomial_Index'Result in Packed_Binomial_Index;
   pragma Inline (Binomial_Index);
   --  C(N,K) = C(N,N-K). Pack only K <= N/2; the preceding rows contain floor((N+1)**2/4) entries.

   function Build_Exact_Binomial_Table return Exact_Binomial_Table_Type is
      use type Interfaces.Unsigned_128;
      Result : Exact_Binomial_Table_Type := [others => 0];
   begin
      --  Use the same Pascal recurrence as the floating-point table, but keep every entry as an integer. The higher
      --  degrees exceed floating-point integer precision, so each scaling factor must be enclosed when converted.
      --  The fallback multiplication can also combine these exact integers before enclosing its product weights.
      for N in 0 .. Maximum_Derivative_Bernstein_Degree loop
         Result (Binomial_Index (N, 0)) := 1;
         for K in 1 .. N / 2 loop
            Result (Binomial_Index (N, K)) :=
              Result (Binomial_Index (N - 1, K - 1)) + Result (Binomial_Index (N - 1, K));
         end loop;
      end loop;
      return Result;
   end Build_Exact_Binomial_Table;

   Exact_Binomial_Table : constant Exact_Binomial_Table_Type := Build_Exact_Binomial_Table;
   --  Elaboration-time cache used by Bernstein multiplication and degree elevation without rounded table entries.

   function Exact_Binomial (N, K : Natural) return Exact_Binomial_Value
   is (if K > N or else N > Maximum_Derivative_Bernstein_Degree
       then 0
       else Exact_Binomial_Table (Binomial_Index (N, K)));

   function Binomial (N, K : Natural) return Dimensionless is
   begin
      return (if K > N or else N > Maximum_Binomial_Degree then 0.0 else Binomial_Table (N, K));
   end Binomial;

   function To_Dimensionless (Value : Exact_Binomial_Value) return Dimensionless
   with Post => To_Dimensionless'Result >= 0.0 and then To_Dimensionless'Result <= 2.0**128;
   --  The floating-point number nearest to Value, with ties to even: exactly what a conversion gives. SPARK has no
   --  conversion from a 128-bit integer, so values of 2⁶⁴ and above are converted in two steps.

   function To_Dimensionless (Value : Exact_Binomial_Value) return Dimensionless is
      use type Interfaces.Unsigned_64;
      use type Interfaces.Unsigned_128;
      Leading : Exact_Binomial_Value := Value;
      Divisor : Exact_Binomial_Value := 1;
      Scale   : Dimensionless := 1.0;
      Kept    : Interfaces.Unsigned_64;
   begin
      if Value < 2**64 then
         return Dimensionless (Interfaces.Unsigned_64 (Value));
      end if;
      --  Keep the leading 64 bits and fold every bit below them into the lowest kept one. The mantissa is at
      --  least two bits shorter than that, so the folded bit decides the rounding of the kept bits exactly as the
      --  discarded ones would have, and the power of two restores the magnitude without rounding again.
      --
      --  Between one and sixty-four bits are discarded. Each step below discards its number of bits when at least
      --  that many remain to be discarded, which leaves at most 32, 16, 8, 4, 2, 1 and finally none.
      if Leading >= 2**95 then
         Leading := Leading / 2**32;
         Divisor := Divisor * 2**32;
         Scale := Scale * 2.0**32;
      end if;
      pragma Assert (Scale >= 1.0 and then Scale <= 2.0**32);
      if Leading >= 2**79 then
         Leading := Leading / 2**16;
         Divisor := Divisor * 2**16;
         Scale := Scale * 2.0**16;
      end if;
      pragma Assert (Scale >= 1.0 and then Scale <= 2.0**48);
      if Leading >= 2**71 then
         Leading := Leading / 2**8;
         Divisor := Divisor * 2**8;
         Scale := Scale * 2.0**8;
      end if;
      pragma Assert (Scale >= 1.0 and then Scale <= 2.0**56);
      if Leading >= 2**67 then
         Leading := Leading / 2**4;
         Divisor := Divisor * 2**4;
         Scale := Scale * 2.0**4;
      end if;
      pragma Assert (Scale >= 1.0 and then Scale <= 2.0**60);
      if Leading >= 2**65 then
         Leading := Leading / 2**2;
         Divisor := Divisor * 2**2;
         Scale := Scale * 2.0**2;
      end if;
      pragma Assert (Scale >= 1.0 and then Scale <= 2.0**62);
      if Leading >= 2**64 then
         Leading := Leading / 2;
         Divisor := Divisor * 2;
         Scale := Scale * 2.0;
      end if;
      pragma Assert (Scale >= 1.0 and then Scale <= 2.0**63);
      if Leading >= 2**64 then
         Leading := Leading / 2;
         Divisor := Divisor * 2;
         Scale := Scale * 2.0;
      end if;
      pragma Assert (Scale >= 1.0 and then Scale <= 2.0**64);
      Kept := Interfaces.Unsigned_64 (Leading);
      if Leading * Divisor /= Value then
         Kept := Kept or 1;
      end if;
      return Dimensionless (Kept) * Scale;
   end To_Dimensionless;

   function Closure_Envelope_Bernstein return Raw_Bernstein is
   begin
      --  The closure envelope is one degree-eight Bernstein basis function:
      --
      --     256·U⁴·(1 - U)⁴ = (128/35)·B₄,₈(U).
      --
      --  Store that coefficient directly, avoiding cancellation in a power conversion.
      return [4 => 128.0 / 35.0, others => 0.0];
   end Closure_Envelope_Bernstein;

   function Closure_Envelope_Value (U : Dimensionless) return Dimensionless is
   begin
      --  The fourth powers are written as squares of squares, which is how the compiler forms them.
      return 256.0 * ((U * U) * (U * U)) * (((1.0 - U) * (1.0 - U)) * ((1.0 - U) * (1.0 - U)));
   end Closure_Envelope_Value;

   function Factorial (N : Natural) return Dimensionless is
      Result : Dimensionless := 1.0;
   begin
      --  The largest requested factorial is small enough to form directly without overflow or a lookup table. No
      --  product reaches Factorial_Limit; taking the smaller of the two states that for the proof.
      for I in 2 .. N loop
         pragma Loop_Invariant (Result >= 1.0 and then Result <= Factorial_Limit);
         Result := Dimensionless'Min (Factorial_Limit, Result * Dimensionless (I));
      end loop;
      return Result;
   end Factorial;

   procedure Set_Power_Coefficient
     (Chart     : in out Power_Chart;
      Index     : Chart_Coefficient_Index;
      Component : Chart_Component_Index;
      Value     : Dimensionless;
      Limit     : Dimensionless)
   with Pre => Within (Chart, Limit) and then abs Value <= Limit, Post => Within (Chart, Limit);
   --  Store a coefficient which is within the limit that the others are within.

   procedure Set_Power_Coefficient
     (Chart     : in out Power_Chart;
      Index     : Chart_Coefficient_Index;
      Component : Chart_Component_Index;
      Value     : Dimensionless;
      Limit     : Dimensionless)
   is
      pragma Unreferenced (Limit);
   begin
      Chart (Index, Component) := Value;
   end Set_Power_Coefficient;
   pragma Inline (Set_Power_Coefficient);

   function Difference_Limit (Order : Chart_Coefficient_Index) return Dimensionless
   is (case Order is
         when 0 => 2.0**560,
         when 1 => 2.0**561,
         when 2 => 2.0**562,
         when 3 => 2.0**563,
         when 4 => 2.0**564,
         when 5 => 2.0**565,
         when 6 => 2.0**566,
         when 7 => 2.0**567,
         when 8 => 2.0**568)
   with Ghost;
   --  Above a forward difference of the given order of controls within Chart_Build_Limit: each order doubles.

   function Power_Column (Controls : Raw_Bernstein) return Raw_Bernstein
   with
     Pre  => (for all J in Raw_Bernstein'Range => abs Controls (J) <= Chart_Build_Limit),
     Post => (for all J in Raw_Bernstein'Range => abs Power_Column'Result (J) <= Power_Basis_Limit);
   --  Power coefficient k is binomial(n, k) times the kth forward difference of the Bernstein controls.

   function Power_Column (Controls : Raw_Bernstein) return Raw_Bernstein is
      Result      : Raw_Bernstein := [others => 0.0];
      Differences : Raw_Bernstein := Controls;
   begin
      for Order in Chart_Coefficient_Index loop
         pragma Loop_Invariant (for all J in Raw_Bernstein'Range => abs Result (J) <= Power_Basis_Limit);
         pragma Loop_Invariant (for all J in Raw_Bernstein'Range => abs Differences (J) <= Difference_Limit (Order));
         pragma Assert (abs Differences (0) <= 2.0**568);
         declare
            Coefficient : constant Dimensionless := Binomial (Fixed_Chart_Degree, Order) * Differences (0);
         begin
            pragma Assert (abs Coefficient <= Power_Basis_Limit);
            Result (Order) := Coefficient;
         end;
         if Order < Fixed_Chart_Degree then
            pragma Assert (Difference_Limit (Order) + Difference_Limit (Order) <= Difference_Limit (Order + 1));
            for Index in 0 .. Fixed_Chart_Degree - Order - 1 loop
               pragma Loop_Invariant (for all J in Raw_Bernstein'Range => abs Result (J) <= Power_Basis_Limit);
               pragma
                 Loop_Invariant
                   (for all J in Raw_Bernstein'Range =>
                      (if J < Index
                       then abs Differences (J) <= Difference_Limit (Order + 1)
                       else abs Differences (J) <= Difference_Limit (Order)));
               pragma Assert (abs Differences (Index + 1) <= Difference_Limit (Order));
               pragma Assert (abs Differences (Index) <= Difference_Limit (Order));
               declare
                  Difference : constant Dimensionless := Differences (Index + 1) - Differences (Index);
               begin
                  pragma Assert (abs Difference <= Difference_Limit (Order) + Difference_Limit (Order));
                  pragma Assert (abs Difference <= Difference_Limit (Order + 1));
                  Differences (Index) := Difference;
               end;
            end loop;
            pragma Assert (for all J in Raw_Bernstein'Range => abs Differences (J) <= Difference_Limit (Order + 1));
         end if;
      end loop;
      return Result;
   end Power_Column;

   function Bernstein_Component (Chart : Bernstein_Chart; Component : Chart_Component_Index) return Raw_Bernstein
   is ([for J in Raw_Bernstein'Range => Chart (J, Component)])
   with
     Pre  => Within (Chart, Chart_Build_Limit),
     Post => (for all J in Raw_Bernstein'Range => abs Bernstein_Component'Result (J) <= Chart_Build_Limit);
   --  The controls of one component of a chart.

   function Power_Basis (Coefficients : Bernstein_Chart) return Power_Chart is
      Result : Power_Chart := [others => [others => 0.0]];
   begin
      pragma Assert (Within (Result, Power_Basis_Limit));
      for Component in Chart_Component_Index loop
         pragma Loop_Invariant (Within (Result, Power_Basis_Limit));
         declare
            Column : constant Raw_Bernstein := Power_Column (Bernstein_Component (Coefficients, Component));
         begin
            for Order in Chart_Coefficient_Index loop
               pragma Loop_Invariant (Within (Result, Power_Basis_Limit));
               pragma Assert (abs Column (Order) <= Power_Basis_Limit);
               Set_Power_Coefficient (Result, Order, Component, Column (Order), Power_Basis_Limit);
            end loop;
         end;
      end loop;
      return Result;
   end Power_Basis;

   function Satisfies_Unit_Tangent_Identities
     (Tangent              : Position_Scale;
      Tangent_Derivative_1 : Tangent_Derivative_1_Vector;
      Tangent_Derivative_2 : Tangent_Derivative_2_Vector;
      Tangent_Derivative_3 : Tangent_Derivative_3_Vector) return Boolean
   is
      Norm_Squared, First_Residual, Second_Residual, Third_Residual : Dimensionless := 0.0;
      Norm_Work, First_Work, Second_Work, Third_Work                : Dimensionless := 0.0;

      Axis_Term_Limit : constant := 2.0**203;
      --  Above what one axis adds to any of the sums: products of two components within the limits tested below.

      function Is_Roundoff (Residual, Work : Dimensionless) return Boolean
      with Pre => abs Residual <= 2.0**206 and then abs Work <= 2.0**206;
      --  Test whether a residual is consistent with accumulated floating-point work.

      function Is_Roundoff (Residual, Work : Dimensionless) return Boolean is
         Tolerance : constant Dimensionless :=
           Jet_Tolerance_Factor * Dimensionless'Model_Epsilon * Dimensionless'Max (1.0, Work);
      begin
         return abs Residual <= Tolerance;
      end Is_Roundoff;
   begin
      for Axis in Axis_Name loop
         pragma
           Loop_Invariant
             (abs Norm_Squared <= Dimensionless (Axis_Name'Pos (Axis)) * Axis_Term_Limit
              and then abs First_Residual <= Dimensionless (Axis_Name'Pos (Axis)) * Axis_Term_Limit
              and then abs Second_Residual <= Dimensionless (Axis_Name'Pos (Axis)) * Axis_Term_Limit
              and then abs Third_Residual <= Dimensionless (Axis_Name'Pos (Axis)) * Axis_Term_Limit
              and then abs Norm_Work <= Dimensionless (Axis_Name'Pos (Axis)) * Axis_Term_Limit
              and then abs First_Work <= Dimensionless (Axis_Name'Pos (Axis)) * Axis_Term_Limit
              and then abs Second_Work <= Dimensionless (Axis_Name'Pos (Axis)) * Axis_Term_Limit
              and then abs Third_Work <= Dimensionless (Axis_Name'Pos (Axis)) * Axis_Term_Limit);
         --  A component beyond these limits belongs to no unit tangent jet of a machine. Within them no product
         --  or sum below can overflow. A NaN fails the tests too.
         if not (abs Tangent (Axis) <= Unit_Component_Limit)
           or else not (abs Tangent_Derivative_1 (Axis) <= Jet_Derivative_Limit / mm)
           or else not (abs Tangent_Derivative_2 (Axis) <= Jet_Derivative_Limit / mm**2)
           or else not (abs Tangent_Derivative_3 (Axis) <= Jet_Derivative_Limit / mm**3)
         then
            return False;
         end if;
         declare
            T     : constant Dimensionless := Tangent (Axis);
            D1    : constant Dimensionless := Tangent_Derivative_1 (Axis) * mm;
            D2    : constant Dimensionless := Tangent_Derivative_2 (Axis) * mm**2;
            D3    : constant Dimensionless := Tangent_Derivative_3 (Axis) * mm**3;
            T_T   : constant Dimensionless := T * T;
            T_D1  : constant Dimensionless := T * D1;
            T_D2  : constant Dimensionless := T * D2;
            D1_D1 : constant Dimensionless := D1 * D1;
            T_D3  : constant Dimensionless := T * D3;
            D1_D2 : constant Dimensionless := D1 * D2;
         begin
            pragma
              Assert
                (abs D1 <= Jet_Derivative_Limit
                 and then abs D2 <= Jet_Derivative_Limit
                 and then abs D3 <= Jet_Derivative_Limit);
            pragma Assert (abs T_T <= 4.0 and then abs T_D1 <= 2.0**101 and then abs T_D2 <= 2.0**101);
            pragma Assert (abs D1_D1 <= 2.0**200 and then abs T_D3 <= 2.0**101 and then abs D1_D2 <= 2.0**200);

            Norm_Squared := Norm_Squared + T_T;
            First_Residual := First_Residual + T_D1;
            Second_Residual := Second_Residual + T_D2 + D1_D1;
            Third_Residual := Third_Residual + T_D3 + 3.0 * D1_D2;

            Norm_Work := Norm_Work + abs T_T;
            First_Work := First_Work + abs T_D1;
            Second_Work := Second_Work + abs T_D2 + abs D1_D1;
            Third_Work := Third_Work + abs T_D3 + 3.0 * abs D1_D2;
         end;
         pragma
           Assert
             (abs Norm_Squared <= Dimensionless (Axis_Name'Pos (Axis) + 1) * Axis_Term_Limit
              and then abs First_Residual <= Dimensionless (Axis_Name'Pos (Axis) + 1) * Axis_Term_Limit
              and then abs Second_Residual <= Dimensionless (Axis_Name'Pos (Axis) + 1) * Axis_Term_Limit
              and then abs Third_Residual <= Dimensionless (Axis_Name'Pos (Axis) + 1) * Axis_Term_Limit);
         pragma
           Assert
             (abs Norm_Work <= Dimensionless (Axis_Name'Pos (Axis) + 1) * Axis_Term_Limit
              and then abs First_Work <= Dimensionless (Axis_Name'Pos (Axis) + 1) * Axis_Term_Limit
              and then abs Second_Work <= Dimensionless (Axis_Name'Pos (Axis) + 1) * Axis_Term_Limit
              and then abs Third_Work <= Dimensionless (Axis_Name'Pos (Axis) + 1) * Axis_Term_Limit);
      end loop;
      pragma
        Assert
          (abs Norm_Squared <= 4.0 * Axis_Term_Limit
           and then abs First_Residual <= 4.0 * Axis_Term_Limit
           and then abs Second_Residual <= 4.0 * Axis_Term_Limit
           and then abs Third_Residual <= 4.0 * Axis_Term_Limit);
      pragma
        Assert
          (abs Norm_Work <= 4.0 * Axis_Term_Limit
           and then abs First_Work <= 4.0 * Axis_Term_Limit
           and then abs Second_Work <= 4.0 * Axis_Term_Limit
           and then abs Third_Work <= 4.0 * Axis_Term_Limit);

      return
        Is_Roundoff (Norm_Squared - 1.0, Norm_Work)
        and then Is_Roundoff (First_Residual, First_Work)
        and then Is_Roundoff (Second_Residual, Second_Work)
        and then Is_Roundoff (Third_Residual, Third_Work);
   end Satisfies_Unit_Tangent_Identities;

   function Safe_Norm (Value : Position_Scale; Success : out Boolean) return Dimensionless is
      Scale : Dimensionless := 0.0;
      Sum   : Dimensionless := 0.0;
   begin
      for A in Axis_Name loop
         --  No vector of the construction approaches the limit, which leaves room for the final product. A NaN
         --  fails the test too.
         if not (abs Value (A) <= Norm_Operand_Limit) then
            Success := False;
            return 0.0;
         end if;
         pragma Assert (if Within (Value, Tame_Limit) then abs Value (A) <= Tame_Limit);
         Scale := Dimensionless'Max (Scale, abs Value (A));
         pragma
           Loop_Invariant
             (Scale >= 0.0
              and then Scale <= Norm_Operand_Limit
              and then (if Within (Value, Tame_Limit) then Scale <= Tame_Limit)
              and then (for all B in Axis_Name => (if B <= A then abs Value (B) <= Scale)));
      end loop;

      if Scale = 0.0 then
         Success := True;
         return 0.0;
      end if;

      --  Scaling by the largest component keeps Σ(Value/Scale)² away from both overflow and underflow.
      for A in Axis_Name loop
         pragma Loop_Invariant (Sum >= 0.0 and then Sum <= Dimensionless (Axis_Name'Pos (A)));
         pragma Assert (abs Value (A) <= Scale);
         declare
            Ratio : constant Dimensionless := Value (A) / Scale;
         begin
            pragma Assert (abs Ratio <= 1.0);
            Sum := Sum + Ratio * Ratio;
         end;
         pragma Assert (Sum >= 0.0 and then Sum <= Dimensionless (Axis_Name'Pos (A) + 1));
      end loop;
      pragma Assert (Sum <= 4.0);
      Stereographic_Kernel.Elementary.Lemma_Sqrt_Is_At_Most (Sum, 2.0);
      Success := True;
      --  The root is at least one, because the largest component adds exactly one to the sum. Taking the larger of
      --  the scale and the product says that the norm is at least every component without an argument about the
      --  rounding of the root.
      declare
         Root : constant Dimensionless := Dimensionless_Math.Sqrt (Sum);
         Norm : constant Dimensionless := Dimensionless'Max (Scale, Scale * Root);
      begin
         pragma Assert (Root >= 0.0 and then Root <= 2.0);
         pragma Assert (Norm <= 2.0 * Scale);
         return Norm;
      end;
   end Safe_Norm;

   function Safe_Norm (Value : Position_Offset; Success : out Boolean) return Length is
      --  Remove the physical unit and measure the dimensionless vector.
      Raw  : constant Position_Scale := [for A in Axis_Name => Dimensionless (Value (A) / mm)];
      Norm : Dimensionless;
   begin
      pragma
        Assert
          (if (for all A in Axis_Name => abs Value (A) <= Tame_Limit * mm)
           then (for all A in Axis_Name => abs Raw (A) <= Tame_Limit));
      Norm := Safe_Norm (Raw, Success);
      pragma Assert (if Success then (for all A in Axis_Name => abs Raw (A) <= Norm));
      pragma Assert (for all A in Axis_Name => Value (A) = Raw (A) * mm);
      pragma Assert (if Success then (for all A in Axis_Name => abs Value (A) <= Norm * mm));
      return Norm * mm;
   end Safe_Norm;

   function Raw_Taylor_Multiply (Left, Right : Raw_Taylor) return Raw_Taylor is
      Result : Raw_Taylor := [others => 0.0];
   begin
      --  Truncated Cauchy product: (Left·Right)ₖ = Σⱼ₌₀ᵏ Leftⱼ·Rightₖ₋ⱼ.
      for I in Result'Range loop
         pragma Loop_Invariant (Within (Result, Taylor_Product_Limit));
         declare
            Sum : Dimensionless := 0.0;
         begin
            for J in 0 .. I loop
               pragma Loop_Invariant (abs Sum <= Dimensionless (J) * 2.0**940);
               pragma Assert (abs Left (J) <= Taylor_Operand_Limit and then abs Right (I - J) <= Taylor_Operand_Limit);
               Sum := Sum + Left (J) * Right (I - J);
               pragma Assert (abs Sum <= Dimensionless (J + 1) * 2.0**940);
            end loop;
            pragma Assert (abs Sum <= 4.0 * 2.0**940);
            Result (I) := Sum;
         end;
      end loop;
      return Result;
   end Raw_Taylor_Multiply;

   function Raw_Taylor_Reciprocal (Value : Raw_Taylor) return Raw_Taylor is
      V0 : constant Dimensionless := Value (0);
      V1 : constant Dimensionless := Value (1);
      V2 : constant Dimensionless := Value (2);
      V3 : constant Dimensionless := Value (3);
      --  Solve Value·Result = 1 coefficient by coefficient:
      --
      --     Result₀ = 1/Value₀,
      --     Resultₖ = -Σⱼ₌₁ᵏ Valueⱼ·Resultₖ₋ⱼ / Value₀.
      --
      --  The constant term is above 10⁻¹⁰ in magnitude, so its reciprocal is below 2³⁴ and each order below is
      --  bounded by the order before it.
      pragma Assert (abs V1 <= Taylor_Divisor_Limit and then abs V2 <= Taylor_Divisor_Limit);
      pragma Assert (abs V3 <= Taylor_Divisor_Limit);
      R0 : constant Dimensionless := 1.0 / V0;
      pragma Assert (abs R0 <= 2.0**34);
      N1 : constant Dimensionless := 0.0 - V1 * R0;
      pragma Assert (abs N1 <= 2.0**144);
      R1 : constant Dimensionless := N1 / V0;
      pragma Assert (abs R1 <= 2.0**178);
      N2 : constant Dimensionless := (0.0 - V1 * R1) - V2 * R0;
      pragma Assert (abs N2 <= 2.0**289);
      R2 : constant Dimensionless := N2 / V0;
      pragma Assert (abs R2 <= 2.0**323);
      N3 : constant Dimensionless := ((0.0 - V1 * R2) - V2 * R1) - V3 * R0;
      pragma Assert (abs N3 <= 2.0**434);
      R3 : constant Dimensionless := N3 / V0;
      pragma Assert (abs R3 <= Taylor_Reciprocal_Limit);
   begin
      return [0 => R0, 1 => R1, 2 => R2, 3 => R3];
   end Raw_Taylor_Reciprocal;

   Jet_Dot_Limit : constant := 2.0**202;
   --  Above the dot product of two vectors within Tame_Limit.

   procedure Add_Dot (Sum : in out Dimensionless; Left, Right : Position_Scale; Dots_Before : Natural)
   with
     Pre  =>
       Dots_Before <= 16
       and then Within (Left, Tame_Limit)
       and then Within (Right, Tame_Limit)
       and then abs Sum <= Dimensionless (Dots_Before) * Jet_Dot_Limit,
     Post => abs Sum <= Dimensionless (Dots_Before + 1) * Jet_Dot_Limit;
   --  Add the products of the components of two vectors to a running sum, one axis after another.

   procedure Add_Dot (Sum : in out Dimensionless; Left, Right : Position_Scale; Dots_Before : Natural) is
   begin
      for A in Axis_Name loop
         pragma
           Loop_Invariant
             (abs Sum <= Dimensionless (Dots_Before) * Jet_Dot_Limit + Dimensionless (Axis_Name'Pos (A)) * 2.0**200);
         pragma Assert (abs Left (A) <= Tame_Limit and then abs Right (A) <= Tame_Limit);
         declare
            Term : constant Dimensionless := Left (A) * Right (A);
            Next : constant Dimensionless := Sum + Term;
         begin
            pragma Assert (abs Term <= 2.0**200);
            pragma
              Assert
                (abs Next
                 <= Dimensionless (Dots_Before) * Jet_Dot_Limit + Dimensionless (Axis_Name'Pos (A) + 1) * 2.0**200);
            Sum := Next;
         end;
      end loop;
      pragma Assert (abs Sum <= Dimensionless (Dots_Before) * Jet_Dot_Limit + 4.0 * 2.0**200);
   end Add_Dot;

   Canonical_Term_Limit : constant := 2.0**568;
   --  Above the product of a jet component within Tame_Limit and a coefficient of a reciprocal series.

   procedure Add_Scaled
     (Sum : in out Position_Scale; Value : Position_Scale; Factor : Dimensionless; Terms_Before : Natural)
   with
     Pre  =>
       Terms_Before <= 3
       and then Within (Value, Tame_Limit)
       and then abs Factor <= Taylor_Reciprocal_Limit
       and then Within (Sum, Dimensionless (Terms_Before) * Canonical_Term_Limit)
       and then (if Terms_Before = 0 then Within (Sum, 0.0)),
     Post =>
       Within (Sum, Dimensionless (Terms_Before + 1) * Canonical_Term_Limit)
       and then
         (if Terms_Before = 0 and then Within (Value, Unit_Component_Limit) and then abs Factor <= 2.0
          then Within (Sum, 4.0));
   --  Add a vector times a scalar to a running sum, one axis after another.

   procedure Add_Scaled
     (Sum : in out Position_Scale; Value : Position_Scale; Factor : Dimensionless; Terms_Before : Natural)
   is
      Before : constant Position_Scale := Sum
      with Ghost;
   begin
      pragma Assert (for all A in Axis_Name => abs Before (A) <= Dimensionless (Terms_Before) * Canonical_Term_Limit);
      for A in Axis_Name loop
         pragma Assert (Sum (A) = Before (A));
         pragma Assert (abs Sum (A) <= Dimensionless (Terms_Before) * Canonical_Term_Limit);
         pragma Assert (abs Value (A) <= Tame_Limit);
         declare
            Term : constant Dimensionless := Value (A) * Factor;
            Next : constant Dimensionless := Sum (A) + Term;
         begin
            pragma Assert (abs Term <= Canonical_Term_Limit);
            pragma Assert (abs Next <= Dimensionless (Terms_Before + 1) * Canonical_Term_Limit);
            pragma
              Assert
                (if Terms_Before = 0 and then Within (Value, Unit_Component_Limit) and then abs Factor <= 2.0
                 then abs Term <= 4.0 and then Sum (A) = 0.0 and then abs Next <= 4.0);
            Sum (A) := Next;
         end;
         pragma
           Loop_Invariant
             (for all B in Axis_Name =>
                (if B <= A
                 then
                   abs Sum (B) <= Dimensionless (Terms_Before + 1) * Canonical_Term_Limit
                   and then
                     (if Terms_Before = 0 and then Within (Value, Unit_Component_Limit) and then abs Factor <= 2.0
                      then abs Sum (B) <= 4.0)
                 else Sum (B) = Before (B)));
      end loop;
   end Add_Scaled;

   procedure Scale_Derivatives
     (Derivative_1 : Curvature;
      Derivative_2 : Curvature_To_2;
      Derivative_3 : Curvature_To_3;
      Chord_Length : Length;
      R1, R2, R3   : out Dimensionless;
      Usable       : out Boolean)
   with
     Pre  => Chord_Length > 0.0 * mm and then Chord_Length <= Chord_Length_Limit,
     Post => abs R1 <= Tame_Limit and then abs R2 <= Tame_Limit and then abs R3 <= Tame_Limit;
   --  The Taylor coefficients L·T′, L²·T″/2! and L³·T‴/3! of one component, or zeros with Usable False when a
   --  derivative or a coefficient is beyond the limits of the construction, as a NaN is.

   procedure Scale_Derivatives
     (Derivative_1 : Curvature;
      Derivative_2 : Curvature_To_2;
      Derivative_3 : Curvature_To_3;
      Chord_Length : Length;
      R1, R2, R3   : out Dimensionless;
      Usable       : out Boolean) is
   begin
      R1 := 0.0;
      R2 := 0.0;
      R3 := 0.0;
      Usable := False;
      --  The identities accepted only derivatives within this limit, which keeps the products below finite. It is
      --  tested again here, where the proof needs it.
      if not (abs Derivative_1 <= Jet_Derivative_Limit / mm)
        or else not (abs Derivative_2 <= Jet_Derivative_Limit / mm**2)
        or else not (abs Derivative_3 <= Jet_Derivative_Limit / mm**3)
      then
         return;
      end if;
      declare
         S1 : constant Dimensionless := Derivative_1 * Chord_Length;
         S2 : constant Dimensionless := (Derivative_2 * Chord_Length / 2.0) * Chord_Length;
         S3 : constant Dimensionless := ((Derivative_3 * Chord_Length / 6.0) * Chord_Length) * Chord_Length;
      begin
         if abs S1 <= Tame_Limit and then abs S2 <= Tame_Limit and then abs S3 <= Tame_Limit then
            R1 := S1;
            R2 := S2;
            R3 := S3;
            Usable := True;
         end if;
      end;
   end Scale_Derivatives;

   procedure Scale_Jet
     (Jet                        : Endpoint_Tangent_Jet;
      Chord_Length               : Length;
      Raw_0, Raw_1, Raw_2, Raw_3 : out Position_Scale;
      Usable                     : out Boolean)
   with
     Pre  => Chord_Length > 0.0 * mm and then Chord_Length <= Chord_Length_Limit,
     Post =>
       Within (Raw_0, Unit_Component_Limit)
       and then Within (Raw_0, Tame_Limit)
       and then Within (Raw_1, Tame_Limit)
       and then Within (Raw_2, Tame_Limit)
       and then Within (Raw_3, Tame_Limit);
   --  Change from physical arc distance S to the dimensionless coordinate U = S/L, where L is the chord length. The
   --  results are Taylor coefficients rather than derivatives:
   --
   --     R(U) = T + L·T′·U + (L²·T″/2!)·U² + (L³·T‴/3!)·U³.
   --
   --  This removes the physical units and puts every endpoint jet in the coordinate system used by construction.
   --  Usable is False if a component of the jet or of the result is beyond the limits of the construction.

   procedure Scale_Jet
     (Jet                        : Endpoint_Tangent_Jet;
      Chord_Length               : Length;
      Raw_0, Raw_1, Raw_2, Raw_3 : out Position_Scale;
      Usable                     : out Boolean)
   is
      R1, R2, R3 : Dimensionless;
      Scaled     : Boolean;
   begin
      Raw_0 := [others => 0.0];
      Raw_1 := [others => 0.0];
      Raw_2 := [others => 0.0];
      Raw_3 := [others => 0.0];
      Usable := False;
      for A in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => abs Raw_0 (B) <= Unit_Component_Limit);
         pragma Loop_Invariant (for all B in Axis_Name => abs Raw_1 (B) <= Tame_Limit);
         pragma Loop_Invariant (for all B in Axis_Name => abs Raw_2 (B) <= Tame_Limit);
         pragma Loop_Invariant (for all B in Axis_Name => abs Raw_3 (B) <= Tame_Limit);
         if not (abs Jet.Tangent (A) <= Unit_Component_Limit) then
            return;
         end if;
         Scale_Derivatives
           (Jet.Tangent_Derivative_1 (A),
            Jet.Tangent_Derivative_2 (A),
            Jet.Tangent_Derivative_3 (A),
            Chord_Length,
            R1,
            R2,
            R3,
            Scaled);
         if not Scaled then
            return;
         end if;
         Raw_0 (A) := Jet.Tangent (A);
         Raw_1 (A) := R1;
         Raw_2 (A) := R2;
         Raw_3 (A) := R3;
      end loop;
      Usable := True;
   end Scale_Jet;

   procedure Lemma_Within_Larger (Value : Position_Scale; Smaller, Larger : Dimensionless)
   with Ghost, Pre => Within (Value, Smaller) and then Smaller <= Larger, Post => Within (Value, Larger);
   --  A vector within a limit is within every larger one.

   procedure Lemma_Within_Larger (Value : Position_Scale; Smaller, Larger : Dimensionless) is null;

   function Taylor_Of (V0, V1, V2, V3 : Dimensionless) return Raw_Taylor
   is ([0 => V0, 1 => V1, 2 => V2, 3 => V3])
   with
     Post =>
       Taylor_Of'Result (0) = V0
       and then Taylor_Of'Result (1) = V1
       and then Taylor_Of'Result (2) = V2
       and then Taylor_Of'Result (3) = V3
       and then
         (if abs V0 <= Taylor_Divisor_Limit
            and then abs V1 <= Taylor_Divisor_Limit
            and then abs V2 <= Taylor_Divisor_Limit
            and then abs V3 <= Taylor_Divisor_Limit
          then Within (Taylor_Of'Result, Taylor_Divisor_Limit));
   --  The series with the given coefficients.

   function Jet_Of (C0, C1, C2, C3 : Position_Scale) return Scaled_Tangent_Jet
   is ([0 => C0, 1 => C1, 2 => C2, 3 => C3])
   with
     Pre  =>
       Within (C0, Tame_Limit)
       and then Within (C1, Tame_Limit)
       and then Within (C2, Tame_Limit)
       and then Within (C3, Tame_Limit),
     Post =>
       Within (Jet_Of'Result, Tame_Limit)
       and then (if Within (C0, 4.0) then Within (Jet_Of'Result (0), 4.0))
       and then
         (if Within (C0, 0.0) and then Within (C1, 0.0) and then Within (C2, 0.0) and then Within (C3, 0.0)
          then
            (for all Order in Scaled_Tangent_Jet'Range =>
               (for all Axis in Axis_Name => Jet_Of'Result (Order) (Axis) = 0.0)));
   --  The jet with the given coefficient vectors.

   function Canonicalize_And_Validate_Jet
     (Jet : Endpoint_Tangent_Jet; Chord_Length : Length; Canonical : out Scaled_Tangent_Jet) return Boolean
   is
      Zero                       : constant Position_Scale := [others => 0.0];
      Raw_0, Raw_1, Raw_2, Raw_3 : Position_Scale;
      Usable                     : Boolean;
      N0, N1, N2, N3             : Dimensionless := 0.0;
      Q0, Q1, Q2, Q3             : Dimensionless;
      Root_Series, Inverse_Root  : Raw_Taylor;
      C0, C1, C2, C3             : Position_Scale := Zero;
   begin
      pragma Assert (Within (Zero, 0.0));
      Canonical := Jet_Of (Zero, Zero, Zero, Zero);

      --  Validate before changing coordinates. Scaling the kth identity residual by Chord_Length**k and then testing
      --  it against an unscaled absolute tolerance floor would make validity depend on chord length; in particular, a
      --  short chord could hide a material longitudinal derivative. The physical check has one fixed unit convention
      --  and is therefore independent of the later construction coordinate.
      if not (Chord_Length > 0.0 * mm)
        or else not (Chord_Length <= Chord_Length_Limit)
        or else
          not Satisfies_Unit_Tangent_Identities
                (Jet.Tangent, Jet.Tangent_Derivative_1, Jet.Tangent_Derivative_2, Jet.Tangent_Derivative_3)
      then
         return False;
      end if;

      Scale_Jet (Jet, Chord_Length, Raw_0, Raw_1, Raw_2, Raw_3, Usable);
      if not Usable then
         return False;
      end if;

      --  Form the Taylor coefficients of the squared norm N(U) = R(U)·R(U):
      --
      --     Nₖ = Σⱼ₌₀ᵏ Rⱼ·Rₖ₋ⱼ.
      Add_Dot (N0, Raw_0, Raw_0, 0);
      Add_Dot (N1, Raw_0, Raw_1, 0);
      Add_Dot (N1, Raw_1, Raw_0, 1);
      Add_Dot (N2, Raw_0, Raw_2, 0);
      Add_Dot (N2, Raw_1, Raw_1, 1);
      Add_Dot (N2, Raw_2, Raw_0, 2);
      Add_Dot (N3, Raw_0, Raw_3, 0);
      Add_Dot (N3, Raw_1, Raw_2, 1);
      Add_Dot (N3, Raw_2, Raw_1, 2);
      Add_Dot (N3, Raw_3, Raw_0, 3);
      pragma Assert (abs N1 <= 2.0**204 and then abs N2 <= 2.0**204 and then abs N3 <= 2.0**204);

      --  The identities put the squared length of the tangent within rounding of one. Anything far from it is
      --  rejected here, where the root and its reciprocal are about to be formed.
      if not (N0 >= 0.25 and then N0 <= 4.0) then
         return False;
      end if;

      --  Recover Q(U) = √N(U) from Q(U)² = N(U):
      --
      --     Q₀ = √N₀
      --     Qₖ = (Nₖ − Σⱼ₌₁ᵏ⁻¹ Qⱼ·Qₖ₋ⱼ) / (2·Q₀).
      Stereographic_Kernel.Elementary.Lemma_Sqrt_Is_At_Least (N0, 0.5);
      Stereographic_Kernel.Elementary.Lemma_Sqrt_Is_At_Most (N0, 2.0);
      Q0 := Dimensionless_Math.Sqrt (N0);
      pragma Assert (Q0 >= 0.5 and then Q0 <= 2.0);
      Q1 := N1 / (2.0 * Q0);
      pragma Assert (abs Q1 <= 2.0**204);
      Q2 := (N2 - Q1 * Q1) / (2.0 * Q0);
      pragma Assert (abs Q2 <= 2.0**409);
      Q3 := ((N3 - Q1 * Q2) - Q2 * Q1) / (2.0 * Q0);

      --  For a unit tangent every coefficient of the root after the first is rounding noise. The reciprocal below
      --  needs them moderate.
      if not (abs Q1 <= Taylor_Divisor_Limit)
        or else not (abs Q2 <= Taylor_Divisor_Limit)
        or else not (abs Q3 <= Taylor_Divisor_Limit)
      then
         return False;
      end if;

      --  Inverse_Root is the truncated Taylor series 1/Q(U).
      Root_Series := Taylor_Of (Q0, Q1, Q2, Q3);
      Inverse_Root := Raw_Taylor_Reciprocal (Root_Series);
      pragma Assert (abs Inverse_Root (0) <= 2.0);
      pragma Assert (for all K in Raw_Taylor'Range => abs Inverse_Root (K) <= Taylor_Reciprocal_Limit);

      --  Canonicalize the complete jet, not just its zeroth-order tangent:
      --
      --     C(U) = R(U)/||R(U)|| = R(U)·Inverse_Root(U).
      --
      --  Thus Cₖ = (1/k!)·dᵏC/dUᵏ at U = 0. Exact valid input is unchanged mathematically; accepted roundoff-sized
      --  discrepancies are projected back onto the unit tangent sphere through order three.
      Add_Scaled (C0, Raw_0, Inverse_Root (0), 0);
      Add_Scaled (C1, Raw_0, Inverse_Root (1), 0);
      Add_Scaled (C1, Raw_1, Inverse_Root (0), 1);
      Add_Scaled (C2, Raw_0, Inverse_Root (2), 0);
      Add_Scaled (C2, Raw_1, Inverse_Root (1), 1);
      Add_Scaled (C2, Raw_2, Inverse_Root (0), 2);
      Add_Scaled (C3, Raw_0, Inverse_Root (3), 0);
      Add_Scaled (C3, Raw_1, Inverse_Root (2), 1);
      Add_Scaled (C3, Raw_2, Inverse_Root (1), 2);
      Add_Scaled (C3, Raw_3, Inverse_Root (0), 3);

      --  The stages which follow take the canonical jet within the common limit.
      if not Within (C1, Tame_Limit) or else not Within (C2, Tame_Limit) or else not Within (C3, Tame_Limit) then
         return False;
      end if;
      Lemma_Within_Larger (C0, 4.0, Tame_Limit);
      Canonical := Jet_Of (C0, C1, C2, C3);
      return True;
   end Canonicalize_And_Validate_Jet;

   function Frame_Coordinate
     (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Value : Position_Scale) return Dimensionless
   is
      Result : Dimensionless := 0.0;
   begin
      for A in Axis_Name loop
         pragma
           Loop_Invariant
             (abs Result <= Dimensionless (Axis_Name'Pos (A)) * (2.0 * Frame_Operand_Limit)
              and then
                (if Within (Value, Tame_Limit)
                 then abs Result <= Dimensionless (Axis_Name'Pos (A)) * (2.0 * Tame_Limit))
              and then
                (if Within (Value, Unit_Component_Limit) then abs Result <= Dimensionless (Axis_Name'Pos (A)) * 4.0));
         pragma Assert (abs Frame (Component) (A) <= Unit_Component_Limit);
         pragma Assert (abs Value (A) <= Frame_Operand_Limit);
         pragma Assert (if Within (Value, Tame_Limit) then abs Value (A) <= Tame_Limit);
         pragma Assert (if Within (Value, Unit_Component_Limit) then abs Value (A) <= Unit_Component_Limit);
         declare
            Term : constant Dimensionless := Frame (Component) (A) * Value (A);
            Sum  : constant Dimensionless := Result + Term;
         begin
            pragma Assert (abs Term <= 2.0 * Frame_Operand_Limit);
            pragma Assert (if Within (Value, Tame_Limit) then abs Term <= 2.0 * Tame_Limit);
            pragma Assert (if Within (Value, Unit_Component_Limit) then abs Term <= 4.0);
            pragma Assert (abs Sum <= Dimensionless (Axis_Name'Pos (A) + 1) * (2.0 * Frame_Operand_Limit));
            pragma
              Assert
                (if Within (Value, Tame_Limit)
                 then abs Sum <= Dimensionless (Axis_Name'Pos (A) + 1) * (2.0 * Tame_Limit));
            pragma
              Assert
                (if Within (Value, Unit_Component_Limit) then abs Sum <= Dimensionless (Axis_Name'Pos (A) + 1) * 4.0);
            Result := Sum;
         end;
      end loop;
      return Result;
   end Frame_Coordinate;

   function Frame_Vector (Frame : Frame_Vector_Array; Components : Raw_Vector_4) return Position_Scale is
      Result : Position_Scale := [others => 0.0];

      function Column (A : Axis_Name) return Dimensionless
      with
        Pre  =>
          (for all C in Frame_Component_Index => abs Frame (C) (A) <= Unit_Component_Limit)
          and then Within (Components, Frame_Operand_Limit),
        Post =>
          abs Column'Result <= Frame_Product_Limit
          and then (if Within (Components, Tame_Limit) then abs Column'Result <= 8.0 * Tame_Limit)
          and then (if Within (Components, Unit_Component_Limit) then abs Column'Result <= 16.0);
      --  The component of the result on one machine axis.

      function Column (A : Axis_Name) return Dimensionless is
         Sum : Dimensionless := 0.0;
      begin
         for C in Frame_Component_Index loop
            pragma
              Loop_Invariant
                (abs Sum <= Dimensionless (C) * (2.0 * Frame_Operand_Limit)
                 and then (if Within (Components, Tame_Limit) then abs Sum <= Dimensionless (C) * (2.0 * Tame_Limit))
                 and then (if Within (Components, Unit_Component_Limit) then abs Sum <= Dimensionless (C) * 4.0));
            pragma Assert (abs Frame (C) (A) <= Unit_Component_Limit);
            pragma Assert (abs Components (C) <= Frame_Operand_Limit);
            pragma Assert (if Within (Components, Tame_Limit) then abs Components (C) <= Tame_Limit);
            pragma
              Assert (if Within (Components, Unit_Component_Limit) then abs Components (C) <= Unit_Component_Limit);
            declare
               Term : constant Dimensionless := Components (C) * Frame (C) (A);
               Next : constant Dimensionless := Sum + Term;
            begin
               pragma Assert (abs Term <= 2.0 * Frame_Operand_Limit);
               pragma Assert (if Within (Components, Tame_Limit) then abs Term <= 2.0 * Tame_Limit);
               pragma Assert (if Within (Components, Unit_Component_Limit) then abs Term <= 4.0);
               pragma Assert (abs Next <= Dimensionless (C + 1) * (2.0 * Frame_Operand_Limit));
               pragma
                 Assert
                   (if Within (Components, Tame_Limit) then abs Next <= Dimensionless (C + 1) * (2.0 * Tame_Limit));
               pragma
                 Assert (if Within (Components, Unit_Component_Limit) then abs Next <= Dimensionless (C + 1) * 4.0);
               Sum := Next;
            end;
         end loop;
         return Sum;
      end Column;
   begin
      --  Each component is the sum over the frame vectors in order, as if all four sums advanced together.
      for A in Axis_Name loop
         pragma
           Loop_Invariant
             (for all B in Axis_Name =>
                (if B < A
                 then
                   abs Result (B) <= Frame_Product_Limit
                   and then (if Within (Components, Tame_Limit) then abs Result (B) <= 8.0 * Tame_Limit)
                   and then (if Within (Components, Unit_Component_Limit) then abs Result (B) <= 16.0)));
         Result (A) := Column (A);
      end loop;
      return Result;
   end Frame_Vector;

   Gram_Schmidt_Limit : constant := 32.0;
   --  Largest magnitude of a component of a vector while frame vectors are removed from it. Removing an
   --  orthonormal component cannot lengthen a vector, so a candidate never comes near it.

   function Without_Component (Value, Direction : Position_Scale; Projection : Dimensionless) return Position_Scale
   with
     Pre  =>
       Within (Value, Gram_Schmidt_Limit)
       and then Within (Direction, Unit_Component_Limit)
       and then abs Projection <= Frame_Product_Limit,
     Post => Within (Without_Component'Result, 2.0**806);
   --  Value less Projection times Direction.

   function Without_Component (Value, Direction : Position_Scale; Projection : Dimensionless) return Position_Scale is
      Result : Position_Scale := [others => 0.0];
   begin
      for A in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => abs Result (B) <= 2.0**806);
         pragma Assert (abs Value (A) <= Gram_Schmidt_Limit and then abs Direction (A) <= Unit_Component_Limit);
         declare
            Component : constant Dimensionless := Value (A) - Direction (A) * Projection;
         begin
            pragma Assert (abs Component <= 2.0**806);
            Result (A) := Component;
         end;
      end loop;
      return Result;
   end Without_Component;

   function Normalized (Value : Position_Scale; Norm : Dimensionless) return Position_Scale
   with Pre => Norm > 0.0 and then Within (Value, Norm), Post => Within (Normalized'Result, 1.0);
   --  Value divided by a norm which is at least each of its components.

   function Normalized (Value : Position_Scale; Norm : Dimensionless) return Position_Scale is
      Result : Position_Scale := [others => 0.0];
   begin
      for A in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => abs Result (B) <= 1.0);
         pragma Assert (abs Value (A) <= Norm);
         declare
            Component : constant Dimensionless := Value (A) / Norm;
         begin
            pragma Assert (abs Component <= 1.0);
            Result (A) := Component;
         end;
      end loop;
      return Result;
   end Normalized;

   procedure Set_Frame_Vector
     (Frame : in out Frame_Vector_Array; Index : Frame_Component_Index; Value : Position_Scale)
   with
     Pre  => Within (Frame, Unit_Component_Limit) and then Within (Value, Unit_Component_Limit),
     Post => Within (Frame, Unit_Component_Limit);

   procedure Set_Frame_Vector
     (Frame : in out Frame_Vector_Array; Index : Frame_Component_Index; Value : Position_Scale) is
   begin
      Frame (Index) := Value;
   end Set_Frame_Vector;

   function Complete_Frame
     (First                                          : Position_Scale;
      Start_Tangent, Finish_Tangent, Chord_Direction : Position_Scale;
      Frame                                          : out Frame_Vector_Array) return Boolean
   is
      Count : Natural := 0;

      procedure Try_Add (Value : Position_Scale)
      with
        Pre  =>
          Count <= Frame_Vector_Array'Length
          and then Within (Frame, Unit_Component_Limit)
          and then Within (Value, Candidate_Component_Limit),
        Post => Count <= Frame_Vector_Array'Length and then Within (Frame, Unit_Component_Limit);

      procedure Try_Add (Value : Position_Scale) is
         V        : Position_Scale := Value;
         Raw_Norm : Dimensionless;
         Success  : Boolean;
      begin
         if Count = Frame_Vector_Array'Length then
            return;
         end if;

         Raw_Norm := Safe_Norm (V, Success);
         if not Success or else Raw_Norm <= 128.0 * Dimensionless'Model_Epsilon then
            return;
         end if;
         Lemma_Within_Larger (V, Candidate_Component_Limit, Gram_Schmidt_Limit);

         --  Two modified Gram-Schmidt passes suppress the loss of orthogonality caused by nearly dependent
         --  geometric candidates.
         for Pass in 1 .. 2 loop
            pragma Loop_Invariant (Within (V, Gram_Schmidt_Limit));
            for I in 0 .. Count - 1 loop
               pragma Loop_Invariant (Within (V, Gram_Schmidt_Limit));
               pragma Assert (Within (Frame (I), Unit_Component_Limit));
               Lemma_Within_Larger (V, Gram_Schmidt_Limit, Frame_Operand_Limit);
               declare
                  Projection : constant Dimensionless := Frame_Coordinate (Frame, I, V);
               begin
                  V := Without_Component (V, Frame (I), Projection);
               end;
               --  The remainder is no longer than the vector was. A remainder beyond the limit is no candidate.
               if not Within (V, Gram_Schmidt_Limit) then
                  return;
               end if;
            end loop;
         end loop;

         Raw_Norm := Safe_Norm (V, Success);
         if Success and then Raw_Norm > 256.0 * Dimensionless'Model_Epsilon then
            declare
               Unit : constant Position_Scale := Normalized (V, Raw_Norm);
            begin
               Lemma_Within_Larger (Unit, 1.0, Unit_Component_Limit);
               Set_Frame_Vector (Frame, Count, Unit);
            end;
            Count := Count + 1;
         end if;
      end Try_Add;

      Zero : constant Position_Scale := [others => 0.0];
   begin
      Frame := [others => Zero];
      pragma Assert (Within (Frame, Unit_Component_Limit));
      --  The geometric directions first, then the machine axes.
      Try_Add (First);
      Try_Add (Start_Tangent);
      Try_Add (Finish_Tangent);
      Try_Add (Chord_Direction);
      Try_Add ([X_Axis => 1.0, others => 0.0]);
      Try_Add ([Y_Axis => 1.0, others => 0.0]);
      Try_Add ([Z_Axis => 1.0, others => 0.0]);
      Try_Add ([E_Axis => 1.0, others => 0.0]);

      if Count /= Frame_Vector_Array'Length then
         return False;
      end if;

      --  Recheck Fᵢ·Fⱼ ≈ δᵢⱼ after construction instead of assuming normalization made the frame trustworthy.
      for I in Frame_Component_Index loop
         for J in Frame_Component_Index loop
            pragma Assert (Within (Frame (I), Unit_Component_Limit) and then Within (Frame (J), Unit_Component_Limit));
            Lemma_Within_Larger (Frame (J), Unit_Component_Limit, Frame_Operand_Limit);
            declare
               Expected : constant Dimensionless := (if I = J then 1.0 else 0.0);
               Actual   : constant Dimensionless := Frame_Coordinate (Frame, I, Frame (J));
            begin
               if abs (Actual - Expected) > Frame_Residual_Tolerance then
                  return False;
               end if;
            end;
         end loop;
      end loop;
      return True;
   end Complete_Frame;

   Chart_Series_Limit : constant := 8.0 * Tame_Limit;
   --  Above a frame coordinate of a vector within Tame_Limit.

   function Frame_Series_Of
     (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Jet : Scaled_Tangent_Jet) return Raw_Taylor
   with
     Pre  => Within (Frame, Unit_Component_Limit) and then Within (Jet, Tame_Limit),
     Post => Within (Frame_Series_Of'Result, Chart_Series_Limit);
   --  The Taylor series of one frame coordinate of the jet.

   function Frame_Series_Of
     (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Jet : Scaled_Tangent_Jet) return Raw_Taylor
   is
      Result : Raw_Taylor := [others => 0.0];
   begin
      pragma Assert (Within (Frame (Component), Unit_Component_Limit));
      for K in Scaled_Tangent_Jet'Range loop
         pragma Loop_Invariant (for all J in Raw_Taylor'Range => abs Result (J) <= Chart_Series_Limit);
         pragma Assert (Within (Jet (K), Tame_Limit));
         Lemma_Within_Larger (Jet (K), Tame_Limit, Frame_Operand_Limit);
         declare
            Coordinate : constant Dimensionless := Frame_Coordinate (Frame, Component, Jet (K));
         begin
            pragma Assert (abs Coordinate <= Chart_Series_Limit);
            Result (K) := Coordinate;
         end;
      end loop;
      return Result;
   end Frame_Series_Of;

   function Chart_Jet_Of (Series_1, Series_2, Series_3 : Raw_Taylor) return Chart_Jet_Array
   is ([for K in Chart_Jet_Array'Range => [1 => Series_1 (K), 2 => Series_2 (K), 3 => Series_3 (K)]])
   with
     Pre  =>
       Within (Series_1, Tame_Limit) and then Within (Series_2, Tame_Limit) and then Within (Series_3, Tame_Limit),
     Post => Within (Chart_Jet_Of'Result, Tame_Limit);
   --  The chart jet whose components have the given Taylor series.

   function Chart_From_Canonical_Jet
     (Frame : Frame_Vector_Array; Jet : Scaled_Tangent_Jet; Result : out Chart_Jet_Array) return Boolean
   is
      Zero        : constant Raw_Taylor := [others => 0.0];
      Pole_Series : constant Raw_Taylor := Frame_Series_Of (Frame, 0, Jet);
      Denominator : Raw_Taylor;
      Inverse_Den : Raw_Taylor;
      Products    : array (Chart_Component_Index) of Raw_Taylor := [others => Zero];
   begin
      pragma Assert (Within (Zero, Tame_Limit));
      Result := Chart_Jet_Of (Zero, Zero, Zero);

      --  Inverse stereographic projection from a unit tangent T in this frame is
      --
      --     Y꜀ = T꜀/(1 + T₀).
      --
      --  Divide the complete Taylor series so endpoint derivatives through order three are preserved.
      pragma Assert (for all K in Raw_Taylor'Range => abs Pole_Series (K) <= Chart_Series_Limit);
      Denominator := Taylor_Of (Pole_Series (0) + 1.0, Pole_Series (1), Pole_Series (2), Pole_Series (3));
      if Denominator (0) <= Minimum_Safe_Reciprocal_Denominator then
         return False;
      end if;
      Inverse_Den := Raw_Taylor_Reciprocal (Denominator);

      for C in Chart_Component_Index loop
         pragma Loop_Invariant (for all B in Chart_Component_Index => Within (Products (B), Tame_Limit));
         declare
            Series  : constant Raw_Taylor := Frame_Series_Of (Frame, C, Jet);
            Product : Raw_Taylor;
         begin
            pragma Assert (Within (Series, Chart_Series_Limit));
            pragma Assert (Within (Series, Taylor_Operand_Limit));
            pragma Assert (Within (Inverse_Den, Taylor_Operand_Limit));
            Product := Raw_Taylor_Multiply (Series, Inverse_Den);
            --  The stages which follow take the chart jet within the common limit.
            if not Within (Product, Tame_Limit) then
               return False;
            end if;
            Products (C) := Product;
         end;
      end loop;
      Result := Chart_Jet_Of (Products (1), Products (2), Products (3));
      return True;
   end Chart_From_Canonical_Jet;

   Scaled_Jet_Limit : constant := 2.0**402;
   --  Above a chart jet coefficient within Tame_Limit times a power of the length ratio up to its cube, and above
   --  the derivative of that product with respect to the ratio.

   Hermite_Base_Limit : constant := 2.0**522;
   --  Above an endpoint Taylor coefficient of the chart in its own coordinate: scaled coefficients combined with
   --  powers of the warp factor up to its cube, or divided by them.

   Hermite_Term_Limit : constant := 2.0**554;
   --  Above a binomial coefficient times a Taylor coefficient within Hermite_Base_Limit.

   Hermite_Control_Limit : constant := 2.0**557;
   --  Above a control of the degree-seven Hermite interpolant, a sum of at most four such terms, and of its
   --  elevation to degree eight.

   procedure Scale_By_Lambda (Jet : Raw_Taylor; Lambda : Dimensionless; Scaled, D_Scaled : out Raw_Taylor)
   with
     Pre  => Lambda >= 1.0 and then Lambda <= Tame_Limit and then Within (Jet, Tame_Limit),
     Post => Within (Scaled, Scaled_Jet_Limit) and then Within (D_Scaled, Scaled_Jet_Limit);
   --  Scaling the normalized distance by λ scales the kth endpoint derivative by λᵏ. Form the analytic λ-derivative
   --  beside it for the closure Jacobian.

   procedure Scale_By_Lambda (Jet : Raw_Taylor; Lambda : Dimensionless; Scaled, D_Scaled : out Raw_Taylor) is
      --  The powers as the compiler forms them: the square, and the cube as the square times the base.
      Lambda_2 : constant Dimensionless := Lambda * Lambda;
      Lambda_3 : constant Dimensionless := Lambda * Lambda_2;
      J0       : constant Dimensionless := Jet (0);
      J1       : constant Dimensionless := Jet (1);
      J2       : constant Dimensionless := Jet (2);
      J3       : constant Dimensionless := Jet (3);
      pragma Assert (abs J0 <= Tame_Limit and then abs J1 <= Tame_Limit);
      pragma Assert (abs J2 <= Tame_Limit and then abs J3 <= Tame_Limit);
      S0       : constant Dimensionless := 1.0 * J0;
      S1       : constant Dimensionless := Lambda * J1;
      S2       : constant Dimensionless := Lambda_2 * J2;
      S3       : constant Dimensionless := Lambda_3 * J3;
      D1       : constant Dimensionless := 1.0 * 1.0 * J1;
      D2       : constant Dimensionless := 2.0 * Lambda * J2;
      D3       : constant Dimensionless := 3.0 * Lambda_2 * J3;
      pragma Assert (abs S0 <= Scaled_Jet_Limit and then abs S1 <= Scaled_Jet_Limit);
      pragma Assert (abs S2 <= Scaled_Jet_Limit and then abs S3 <= Scaled_Jet_Limit);
      pragma Assert (abs D1 <= Scaled_Jet_Limit and then abs D2 <= Scaled_Jet_Limit);
      pragma Assert (abs D3 <= Scaled_Jet_Limit);
   begin
      Scaled := [0 => S0, 1 => S1, 2 => S2, 3 => S3];
      D_Scaled := [0 => 0.0, 1 => D1, 2 => D2, 3 => D3];
   end Scale_By_Lambda;

   function Start_Base_Of (Scaled : Raw_Taylor; Warp_Factor : Dimensionless) return Raw_Taylor
   with
     Pre  =>
       Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Within (Scaled, Scaled_Jet_Limit),
     Post =>
       Within (Start_Base_Of'Result, Hermite_Base_Limit)
       and then Start_Base_Of'Result (0) = Scaled (0)
       and then
         (if Scaled (1) = 0.0 and then Scaled (2) = 0.0 and then Scaled (3) = 0.0
          then
            Start_Base_Of'Result (1) = 0.0
            and then Start_Base_Of'Result (2) = 0.0
            and then Start_Base_Of'Result (3) = 0.0);
   --  The Taylor coefficients at the start in the chart coordinate V, where
   --
   --     V = W·U/(1 + (W - 1)·U).
   --
   --  Inverse-composing the requested U jets makes the composed tangent match every endpoint derivative through
   --  order three.

   function Start_Base_Of (Scaled : Raw_Taylor; Warp_Factor : Dimensionless) return Raw_Taylor is
      Warp_Difference : constant Dimensionless := Warp_Factor - 1.0;
      Warp_2          : constant Dimensionless := Warp_Factor * Warp_Factor;
      Warp_3          : constant Dimensionless := Warp_2 * Warp_Factor;
      S0              : constant Dimensionless := Scaled (0);
      S1              : constant Dimensionless := Scaled (1);
      S2              : constant Dimensionless := Scaled (2);
      S3              : constant Dimensionless := Scaled (3);
      pragma Assert (abs S0 <= Scaled_Jet_Limit and then abs S1 <= Scaled_Jet_Limit);
      pragma Assert (abs S2 <= Scaled_Jet_Limit and then abs S3 <= Scaled_Jet_Limit);
      pragma Assert (Warp_2 >= 2.0**(-48) and then Warp_3 >= 2.0**(-72));
      B1              : constant Dimensionless := S1 / Warp_Factor;
      N2              : constant Dimensionless := S2 + Warp_Difference * S1;
      B2              : constant Dimensionless := N2 / Warp_2;
      N3              : constant Dimensionless :=
        S3 + 2.0 * Warp_Difference * S2 + Warp_Difference * Warp_Difference * S1;
      B3              : constant Dimensionless := N3 / Warp_3;
      pragma Assert (abs B1 <= 2.0**426);
      pragma Assert (abs N2 <= 2.0**427 and then abs B2 <= 2.0**475);
      pragma Assert (abs N3 <= 2.0**450 and then abs B3 <= Hermite_Base_Limit);
   begin
      return [0 => S0, 1 => B1, 2 => B2, 3 => B3];
   end Start_Base_Of;

   function Finish_Base_Of (Scaled : Raw_Taylor; Warp_Factor : Dimensionless) return Raw_Taylor
   with
     Pre  =>
       Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Within (Scaled, Scaled_Jet_Limit),
     Post =>
       Within (Finish_Base_Of'Result, Hermite_Base_Limit)
       and then Finish_Base_Of'Result (0) = Scaled (0)
       and then
         (if Scaled (1) = 0.0 and then Scaled (2) = 0.0 and then Scaled (3) = 0.0
          then
            Finish_Base_Of'Result (1) = 0.0
            and then Finish_Base_Of'Result (2) = 0.0
            and then Finish_Base_Of'Result (3) = 0.0);
   --  The Taylor coefficients at the finish in the chart coordinate V.

   function Finish_Base_Of (Scaled : Raw_Taylor; Warp_Factor : Dimensionless) return Raw_Taylor is
      Warp_Difference : constant Dimensionless := Warp_Factor - 1.0;
      Warp_2          : constant Dimensionless := Warp_Factor * Warp_Factor;
      Warp_3          : constant Dimensionless := Warp_2 * Warp_Factor;
      F0              : constant Dimensionless := Scaled (0);
      F1              : constant Dimensionless := Scaled (1);
      F2              : constant Dimensionless := Scaled (2);
      F3              : constant Dimensionless := Scaled (3);
      pragma Assert (abs F0 <= Scaled_Jet_Limit and then abs F1 <= Scaled_Jet_Limit);
      pragma Assert (abs F2 <= Scaled_Jet_Limit and then abs F3 <= Scaled_Jet_Limit);
      B1              : constant Dimensionless := Warp_Factor * F1;
      B2              : constant Dimensionless := Warp_2 * F2 + Warp_Factor * Warp_Difference * F1;
      B3              : constant Dimensionless :=
        Warp_3 * F3 + 2.0 * Warp_2 * Warp_Difference * F2 + Warp_Factor * (Warp_Difference * Warp_Difference) * F1;
      pragma Assert (abs B1 <= 2.0**426);
      pragma Assert (abs B2 <= 2.0**452);
      pragma Assert (abs B3 <= Hermite_Base_Limit);
   begin
      return [0 => F0, 1 => B1, 2 => B2, 3 => B3];
   end Finish_Base_Of;

   procedure Set_Hermite_Control (Controls : in out Raw_Bernstein_7; Index : Natural; Value : Dimensionless)
   with
     Pre  =>
       Index <= 7
       and then abs Value <= Hermite_Control_Limit
       and then (for all J in Raw_Bernstein_7'Range => abs Controls (J) <= Hermite_Control_Limit),
     Post => (for all J in Raw_Bernstein_7'Range => abs Controls (J) <= Hermite_Control_Limit);

   procedure Set_Hermite_Control (Controls : in out Raw_Bernstein_7; Index : Natural; Value : Dimensionless) is
   begin
      Controls (Index) := Value;
   end Set_Hermite_Control;

   function Hermite_Controls (Start_Base, Finish_Base : Raw_Taylor) return Raw_Bernstein_7
   with
     Pre  => Within (Start_Base, Hermite_Base_Limit) and then Within (Finish_Base, Hermite_Base_Limit),
     Post => (for all J in Raw_Bernstein_7'Range => abs Hermite_Controls'Result (J) <= Hermite_Control_Limit);
   --  A degree-7 Hermite interpolant has exactly eight endpoint conditions: value plus three derivatives at each
   --  end. For factorial-scaled Taylor data tₖ, the endpoint Bernstein controls satisfy
   --
   --     bᵢ = Σₖ₌₀ⁱ C(i, k)·tₖ/C(7, k).

   function Hermite_Controls (Start_Base, Finish_Base : Raw_Taylor) return Raw_Bernstein_7 is
      Result : Raw_Bernstein_7 := [others => 0.0];
   begin
      for I in 0 .. 3 loop
         pragma Loop_Invariant (for all J in Raw_Bernstein_7'Range => abs Result (J) <= Hermite_Control_Limit);
         declare
            Start_Sum, Finish_Sum : Dimensionless := 0.0;
         begin
            for K in 0 .. I loop
               pragma
                 Loop_Invariant
                   (abs Start_Sum <= Dimensionless (K) * Hermite_Term_Limit
                    and then abs Finish_Sum <= Dimensionless (K) * Hermite_Term_Limit);
               pragma
                 Assert (abs Start_Base (K) <= Hermite_Base_Limit and then abs Finish_Base (K) <= Hermite_Base_Limit);
               pragma Assert (Binomial (7, K) >= 1.0 and then Binomial (I, K) <= Binomial_Limit);
               declare
                  Start_Term  : constant Dimensionless := Binomial (I, K) * Start_Base (K) / Binomial (7, K);
                  Finish_Term : constant Dimensionless :=
                    (if K mod 2 = 0 then 1.0 else -1.0) * Binomial (I, K) * Finish_Base (K) / Binomial (7, K);
                  Next_Start  : constant Dimensionless := Start_Sum + Start_Term;
                  Next_Finish : constant Dimensionless := Finish_Sum + Finish_Term;
               begin
                  pragma Assert (abs Start_Term <= Hermite_Term_Limit and then abs Finish_Term <= Hermite_Term_Limit);
                  pragma Assert (abs Next_Start <= Dimensionless (K + 1) * Hermite_Term_Limit);
                  pragma Assert (abs Next_Finish <= Dimensionless (K + 1) * Hermite_Term_Limit);
                  Start_Sum := Next_Start;
                  Finish_Sum := Next_Finish;
               end;
            end loop;
            pragma
              Assert (abs Start_Sum <= 4.0 * Hermite_Term_Limit and then abs Finish_Sum <= 4.0 * Hermite_Term_Limit);
            pragma Assert (abs Start_Sum <= Hermite_Control_Limit and then abs Finish_Sum <= Hermite_Control_Limit);
            Set_Hermite_Control (Result, I, Start_Sum);
            Set_Hermite_Control (Result, 7 - I, Finish_Sum);
         end;
      end loop;
      return Result;
   end Hermite_Controls;

   function Elevate_To_Chart_Degree (Degree_7 : Raw_Bernstein_7) return Raw_Bernstein
   with
     Pre  => (for all J in Raw_Bernstein_7'Range => abs Degree_7 (J) <= Hermite_Control_Limit),
     Post =>
       (for all J in Raw_Bernstein'Range => abs Elevate_To_Chart_Degree'Result (J) <= 2.0 * Hermite_Control_Limit);
   --  Elevating degree seven to eight only mixes adjacent controls. The weights J/8 and (8 - J)/8 are exact dyadic
   --  numbers, so no binomial lookup or general convolution is needed.

   function Elevate_To_Chart_Degree (Degree_7 : Raw_Bernstein_7) return Raw_Bernstein is
      Result : Raw_Bernstein := [others => 0.0];
   begin
      Result (0) := Degree_7 (0);
      Result (Fixed_Chart_Degree) := Degree_7 (7);
      for J in 1 .. Fixed_Chart_Degree - 1 loop
         pragma Loop_Invariant (for all I in Raw_Bernstein'Range => abs Result (I) <= 2.0 * Hermite_Control_Limit);
         pragma
           Assert (abs Degree_7 (J - 1) <= Hermite_Control_Limit and then abs Degree_7 (J) <= Hermite_Control_Limit);
         declare
            Left_Weight  : constant Dimensionless := Dimensionless (J) / 8.0;
            Right_Weight : constant Dimensionless := Dimensionless (8 - J) / 8.0;
            Control      : constant Dimensionless := Left_Weight * Degree_7 (J - 1) + Right_Weight * Degree_7 (J);
         begin
            pragma Assert (Left_Weight >= 0.0 and then Left_Weight <= 1.0);
            pragma Assert (Right_Weight >= 0.0 and then Right_Weight <= 1.0);
            pragma Assert (abs Control <= 2.0 * Hermite_Control_Limit);
            Result (J) := Control;
         end;
      end loop;
      return Result;
   end Elevate_To_Chart_Degree;

   procedure Build_Chart_Component
     (Start_Jet, Finish_Jet : Raw_Taylor;
      Lambda, C0            : Dimensionless;
      Warp_Factor           : Dimensionless;
      Controls, D_Controls  : out Raw_Bernstein)
   with
     Pre  =>
       Lambda >= 1.0
       and then Lambda <= Tame_Limit
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then abs C0 <= Tame_Limit
       and then Within (Start_Jet, Tame_Limit)
       and then Within (Finish_Jet, Tame_Limit),
     Post =>
       (for all J in Raw_Bernstein'Range => abs Controls (J) <= Chart_Build_Limit)
       and then (for all J in Raw_Bernstein'Range => abs D_Controls (J) <= Chart_Build_Limit);
   --  One component of the chart and of its derivative with respect to Lambda.

   procedure Build_Chart_Component
     (Start_Jet, Finish_Jet : Raw_Taylor;
      Lambda, C0            : Dimensionless;
      Warp_Factor           : Dimensionless;
      Controls, D_Controls  : out Raw_Bernstein)
   is
      Envelope                        : constant Raw_Bernstein := Closure_Envelope_Bernstein;
      Start_Scaled, Finish_Scaled     : Raw_Taylor;
      D_Start_Scaled, D_Finish_Scaled : Raw_Taylor;
      Start_Base, Finish_Base         : Raw_Taylor;
      D_Start_Base, D_Finish_Base     : Raw_Taylor;
      Elevated, D_Elevated            : Raw_Bernstein;
   begin
      Scale_By_Lambda (Start_Jet, Lambda, Start_Scaled, D_Start_Scaled);
      Scale_By_Lambda (Finish_Jet, Lambda, Finish_Scaled, D_Finish_Scaled);
      Start_Base := Start_Base_Of (Start_Scaled, Warp_Factor);
      D_Start_Base := Start_Base_Of (D_Start_Scaled, Warp_Factor);
      Finish_Base := Finish_Base_Of (Finish_Scaled, Warp_Factor);
      D_Finish_Base := Finish_Base_Of (D_Finish_Scaled, Warp_Factor);
      Elevated := Elevate_To_Chart_Degree (Hermite_Controls (Start_Base, Finish_Base));
      D_Elevated := Elevate_To_Chart_Degree (Hermite_Controls (D_Start_Base, D_Finish_Base));
      pragma Assert (abs Start_Base (0) <= Hermite_Base_Limit and then abs D_Start_Base (0) <= Hermite_Base_Limit);
      pragma Assert (abs Finish_Base (0) <= Hermite_Base_Limit and then abs D_Finish_Base (0) <= Hermite_Base_Limit);

      --  Generic degree elevation is mathematically endpoint-jet preserving, but its rounded multiply/add
      --  sequence need not leave controls which should be identical bit-for-bit. Preserve structural zero
      --  endpoint jets in the production chart itself. Besides making the requested derivatives exact for this
      --  common line-line case, this exposes the V⁴ and (1 - V)⁴ factors used by the strict no-bulge certificate
      --  below. Apply the same canonicalization to the analytic lambda derivative so the closure Jacobian
      --  describes exactly the chart being solved.
      if (for all K in 1 .. 3 => Start_Base (K) = 0.0) then
         for J in 0 .. 3 loop
            pragma
              Loop_Invariant (for all I in Raw_Bernstein'Range => abs Elevated (I) <= 2.0 * Hermite_Control_Limit);
            pragma
              Loop_Invariant (for all I in Raw_Bernstein'Range => abs D_Elevated (I) <= 2.0 * Hermite_Control_Limit);
            Elevated (J) := Start_Base (0);
            D_Elevated (J) := D_Start_Base (0);
         end loop;
      end if;
      if (for all K in 1 .. 3 => Finish_Base (K) = 0.0) then
         for J in 5 .. Fixed_Chart_Degree loop
            pragma
              Loop_Invariant (for all I in Raw_Bernstein'Range => abs Elevated (I) <= 2.0 * Hermite_Control_Limit);
            pragma
              Loop_Invariant (for all I in Raw_Bernstein'Range => abs D_Elevated (I) <= 2.0 * Hermite_Control_Limit);
            Elevated (J) := Finish_Base (0);
            D_Elevated (J) := D_Finish_Base (0);
         end loop;
      end if;

      Controls := [others => 0.0];
      D_Controls := D_Elevated;
      --  Add C₀·256·V⁴·(1 - V)⁴. Its fourfold zeros leave every endpoint jet through order three unchanged.
      for J in Chart_Coefficient_Index loop
         pragma Loop_Invariant (for all I in Raw_Bernstein'Range => abs Controls (I) <= Chart_Build_Limit);
         pragma Assert (abs Elevated (J) <= 2.0 * Hermite_Control_Limit);
         pragma Assert (Envelope (J) >= 0.0 and then Envelope (J) <= 4.0);
         declare
            Control : constant Dimensionless := Elevated (J) + Dimensionless (Envelope (J) * C0);
         begin
            pragma Assert (abs Control <= Chart_Build_Limit);
            Controls (J) := Control;
         end;
      end loop;
   end Build_Chart_Component;

   function Taylor_Component (Jets : Chart_Jet_Array; Component : Chart_Component_Index) return Raw_Taylor
   is ([for K in Raw_Taylor'Range => Jets (K) (Component)])
   with Pre => Within (Jets, Tame_Limit), Post => Within (Taylor_Component'Result, Tame_Limit);
   --  The Taylor series of one component of a chart jet.

   procedure Set_Chart_Component
     (Chart : in out Bernstein_Chart; Component : Chart_Component_Index; Controls : Raw_Bernstein)
   with
     Pre  =>
       Within (Chart, Chart_Build_Limit)
       and then (for all J in Raw_Bernstein'Range => abs Controls (J) <= Chart_Build_Limit),
     Post => Within (Chart, Chart_Build_Limit);

   procedure Set_Chart_Component
     (Chart : in out Bernstein_Chart; Component : Chart_Component_Index; Controls : Raw_Bernstein) is
   begin
      for J in Chart_Coefficient_Index loop
         pragma
           Loop_Invariant
             (for all I in Chart_Coefficient_Index =>
                (for all C in Chart_Component_Index => abs Chart (I, C) <= Chart_Build_Limit));
         pragma Assert (abs Controls (J) <= Chart_Build_Limit);
         Chart (J, Component) := Controls (J);
      end loop;
   end Set_Chart_Component;

   procedure Build_Chart
     (Start_Jets, Finish_Jets             : Chart_Jet_Array;
      Lambda                              : Dimensionless;
      C0                                  : Raw_Vector_3;
      Warp_Factor                         : Dimensionless;
      Coefficients, D_Lambda_Coefficients : out Bernstein_Chart)
   is
      Controls, D_Controls : Raw_Bernstein;
   begin
      Coefficients := [others => [others => 0.0]];
      D_Lambda_Coefficients := [others => [others => 0.0]];
      pragma Assert (Within (Coefficients, Chart_Build_Limit));
      pragma Assert (Within (D_Lambda_Coefficients, Chart_Build_Limit));

      for C in Chart_Component_Index loop
         pragma Loop_Invariant (Within (Coefficients, Chart_Build_Limit));
         pragma Loop_Invariant (Within (D_Lambda_Coefficients, Chart_Build_Limit));
         pragma Assert (abs C0 (C) <= Tame_Limit);
         Build_Chart_Component
           (Taylor_Component (Start_Jets, C),
            Taylor_Component (Finish_Jets, C),
            Lambda,
            C0 (C),
            Warp_Factor,
            Controls,
            D_Controls);
         Set_Chart_Component (Coefficients, C, Controls);
         Set_Chart_Component (D_Lambda_Coefficients, C, D_Controls);
      end loop;
   end Build_Chart;

   procedure Build_Chart
     (Start_Jets, Finish_Jets : Chart_Jet_Array;
      Lambda                  : Dimensionless;
      C0                      : Raw_Vector_3;
      Warp_Factor             : Dimensionless;
      Coefficients            : out Bernstein_Chart)
   is
      Ignored : Bernstein_Chart;
   begin
      Build_Chart (Start_Jets, Finish_Jets, Lambda, C0, Warp_Factor, Coefficients, Ignored);
   end Build_Chart;

   function Chart_Point (Y1, Y2, Y3 : Dimensionless) return Chart_Vector
   is ([1 => Y1, 2 => Y2, 3 => Y3])
   with
     Pre  => abs Y1 <= Chart_Point_Limit and then abs Y2 <= Chart_Point_Limit and then abs Y3 <= Chart_Point_Limit,
     Post => Within (Chart_Point'Result, Chart_Point_Limit);
   --  The chart point with the given coordinates.

   function Chart_Value (Coefficients : Bernstein_Chart; U : Dimensionless) return Chart_Vector is
      R1, R2, R3 : Dimensionless := 0.0;
      Basis      : Dimensionless := 1.0;

      procedure Add (I : Chart_Coefficient_Index; Terms_Before : Natural)
      with
        Pre  =>
          Within (Coefficients, Tame_Limit)
          and then Terms_Before <= Fixed_Chart_Degree
          and then Basis >= 0.0
          and then Basis <= 1.0
          and then abs R1 <= Dimensionless (Terms_Before) * Tame_Limit
          and then abs R2 <= Dimensionless (Terms_Before) * Tame_Limit
          and then abs R3 <= Dimensionless (Terms_Before) * Tame_Limit,
        Post =>
          abs R1 <= Dimensionless (Terms_Before + 1) * Tame_Limit
          and then abs R2 <= Dimensionless (Terms_Before + 1) * Tame_Limit
          and then abs R3 <= Dimensionless (Terms_Before + 1) * Tame_Limit;
      --  Add the current basis value times control I to each component.

      procedure Add (I : Chart_Coefficient_Index; Terms_Before : Natural) is
         pragma Unreferenced (Terms_Before);
         C1 : constant Dimensionless := Coefficients (I, 1);
         C2 : constant Dimensionless := Coefficients (I, 2);
         C3 : constant Dimensionless := Coefficients (I, 3);
         pragma Assert (abs C1 <= Tame_Limit and then abs C2 <= Tame_Limit and then abs C3 <= Tame_Limit);
         T1 : constant Dimensionless := Basis * C1;
         T2 : constant Dimensionless := Basis * C2;
         T3 : constant Dimensionless := Basis * C3;
      begin
         pragma Assert (abs T1 <= Tame_Limit);
         pragma Assert (abs T2 <= Tame_Limit);
         pragma Assert (abs T3 <= Tame_Limit);
         R1 := R1 + T1;
         R2 := R2 + T2;
         R3 := R3 + T3;
      end Add;
   begin
      pragma
        Assert
          (abs Coefficients (0, 1) <= Tame_Limit
           and then abs Coefficients (0, 2) <= Tame_Limit
           and then abs Coefficients (0, 3) <= Tame_Limit);
      pragma
        Assert
          (abs Coefficients (Fixed_Chart_Degree, 1) <= Tame_Limit
           and then abs Coefficients (Fixed_Chart_Degree, 2) <= Tame_Limit
           and then abs Coefficients (Fixed_Chart_Degree, 3) <= Tame_Limit);
      if U <= 0.0 then
         return Chart_Point (Coefficients (0, 1), Coefficients (0, 2), Coefficients (0, 3));
      elsif U >= 1.0 then
         return
           Chart_Point
             (Coefficients (Fixed_Chart_Degree, 1),
              Coefficients (Fixed_Chart_Degree, 2),
              Coefficients (Fixed_Chart_Degree, 3));
      elsif U <= 0.5 then
         declare
            One_Minus_U : constant Dimensionless := 1.0 - U;
            Ratio       : constant Dimensionless := U / One_Minus_U;
         begin
            pragma Assert (Ratio >= 0.0 and then Ratio <= 1.0);
            --  Generate Bᵢ,ₙ(U) forward from B₀,ₙ = (1 - U)ⁿ:
            --
            --     Bᵢ₊₁,ₙ/Bᵢ,ₙ = (n - i)/(i + 1) · U/(1 - U).
            --
            --  A basis value never exceeds one. Taking the smaller of one and each product states that for the
            --  proof without an argument about the rounding of the recurrence.
            for I in 1 .. Fixed_Chart_Degree loop
               pragma Loop_Invariant (Basis >= 0.0 and then Basis <= 1.0);
               Basis := Basis * One_Minus_U;
            end loop;
            for I in Chart_Coefficient_Index loop
               pragma Loop_Invariant (Basis >= 0.0 and then Basis <= 1.0);
               pragma
                 Loop_Invariant
                   (abs R1 <= Dimensionless (I) * Tame_Limit
                    and then abs R2 <= Dimensionless (I) * Tame_Limit
                    and then abs R3 <= Dimensionless (I) * Tame_Limit);
               Add (I, I);
               if I < Fixed_Chart_Degree then
                  Basis :=
                    Dimensionless'Min
                      (1.0, Basis * Dimensionless (Fixed_Chart_Degree - I) / Dimensionless (I + 1) * Ratio);
               end if;
            end loop;
         end;
      else
         declare
            Ratio : constant Dimensionless := (1.0 - U) / U;
         begin
            pragma Assert (Ratio >= 0.0 and then Ratio <= 1.0);
            --  Near U = 1, run the reciprocal recurrence backward from Bₙ,ₙ = Uⁿ to avoid a large U/(1 - U).
            for I in 1 .. Fixed_Chart_Degree loop
               pragma Loop_Invariant (Basis >= 0.0 and then Basis <= 1.0);
               Basis := Basis * U;
            end loop;
            for I in reverse Chart_Coefficient_Index loop
               pragma Loop_Invariant (Basis >= 0.0 and then Basis <= 1.0);
               pragma
                 Loop_Invariant
                   (abs R1 <= Dimensionless (Fixed_Chart_Degree - I) * Tame_Limit
                    and then abs R2 <= Dimensionless (Fixed_Chart_Degree - I) * Tame_Limit
                    and then abs R3 <= Dimensionless (Fixed_Chart_Degree - I) * Tame_Limit);
               Add (I, Fixed_Chart_Degree - I);
               if I > 0 then
                  Basis :=
                    Dimensionless'Min
                      (1.0, Basis * Dimensionless (I) / Dimensionless (Fixed_Chart_Degree - I + 1) * Ratio);
               end if;
            end loop;
         end;
      end if;
      pragma
        Assert (abs R1 <= 9.0 * Tame_Limit and then abs R2 <= 9.0 * Tame_Limit and then abs R3 <= 9.0 * Tame_Limit);
      return Chart_Point (R1, R2, R3);
   end Chart_Value;

   function Stereographic (Y : Chart_Vector) return Raw_Vector_4 is
      Y1          : constant Dimensionless := Y (1);
      Y2          : constant Dimensionless := Y (2);
      Y3          : constant Dimensionless := Y (3);
      pragma
        Assert (abs Y1 <= Chart_Point_Limit and then abs Y2 <= Chart_Point_Limit and then abs Y3 <= Chart_Point_Limit);
      R2          : constant Dimensionless := 0.0 + Y1 * Y1 + Y2 * Y2 + Y3 * Y3;
      pragma Assert (R2 >= 0.0 and then R2 <= 2.0**212);
      Denominator : constant Dimensionless := 1.0 + R2;
      Numerator   : constant Dimensionless := 1.0 - R2;
      pragma Assert (Denominator >= 1.0 and then abs Numerator <= Denominator);
      --  Map ℝ³ to the unit 3-sphere:
      --
      --     S(Y) = ((1 - ‖Y‖²), 2Y)/(1 + ‖Y‖²).
      S0          : constant Dimensionless := Numerator / Denominator;
      S1          : constant Dimensionless := 2.0 * Y1 / Denominator;
      S2          : constant Dimensionless := 2.0 * Y2 / Denominator;
      S3          : constant Dimensionless := 2.0 * Y3 / Denominator;
      pragma Assert (abs S0 <= 1.0);
      pragma Assert (abs S1 <= 2.0 * Chart_Point_Limit and then abs S2 <= 2.0 * Chart_Point_Limit);
      pragma Assert (abs S3 <= 2.0 * Chart_Point_Limit);
   begin
      return [0 => S0, 1 => S1, 2 => S2, 3 => S3];
   end Stereographic;

   function Stereographic_Jacobian (Y : Chart_Vector) return Raw_Stereo_Jacobian is
      Result      : Raw_Stereo_Jacobian := [others => [others => 0.0]];
      Y1          : constant Dimensionless := Y (1);
      Y2          : constant Dimensionless := Y (2);
      Y3          : constant Dimensionless := Y (3);
      pragma
        Assert (abs Y1 <= Chart_Point_Limit and then abs Y2 <= Chart_Point_Limit and then abs Y3 <= Chart_Point_Limit);
      R2          : constant Dimensionless := 0.0 + Y1 * Y1 + Y2 * Y2 + Y3 * Y3;
      pragma Assert (R2 >= 0.0 and then R2 <= 2.0**212);
      Denominator : constant Dimensionless := 1.0 + R2;
      Square      : constant Dimensionless := Denominator * Denominator;
      pragma Assert (Denominator >= 1.0 and then Square >= 1.0 and then Square <= 2.0**426);
   begin
      --  Differentiate S₀ = (1 - r²)/(1 + r²) and Sᵢ = 2Yᵢ/(1 + r²) with respect to each Yⱼ.
      for J in Chart_Component_Index loop
         pragma Assert (abs Y (J) <= Chart_Point_Limit);
         Result (0, J) := -4.0 * Y (J) / Square;
      end loop;
      for I in Chart_Component_Index loop
         for J in Chart_Component_Index loop
            pragma Assert (abs Y (I) <= Chart_Point_Limit and then abs Y (J) <= Chart_Point_Limit);
            Result (I, J) := (if I = J then 2.0 / Denominator else 0.0) - 4.0 * Y (I) * Y (J) / Square;
         end loop;
      end loop;
      return Result;
   end Stereographic_Jacobian;

   function Warp_Parameter (U, Warp_Factor : Dimensionless) return Dimensionless is
   begin
      if U <= 0.0 then
         return 0.0;
      elsif U >= 1.0 then
         return 1.0;
      else
         declare
            Result : constant Dimensionless := Warp_Factor * U / ((1.0 - U) + Warp_Factor * U);
         begin
            return Dimensionless'Max (0.0, Dimensionless'Min (1.0, Result));
         end;
      end if;
   end Warp_Parameter;

   function Tangent_At
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Warp_Factor, U : Dimensionless) return Position_Scale
   is
      V     : constant Dimensionless := Warp_Parameter (U, Warp_Factor);
      Point : constant Raw_Vector_4 := Stereographic (Chart_Value (Coefficients, V));
   begin
      pragma Assert (Within (Point, 2.0 * Chart_Point_Limit));
      pragma Assert (Within (Point, Frame_Operand_Limit));
      return Frame_Vector (Frame, Point);
   end Tangent_At;

   function Inflate_Finite (Value : Dimensionless; Upward : Boolean) return Dimensionless
   with
     Pre  => abs Value <= Dimensionless'Last / 2.0,
     Post => (if Upward then Inflate_Finite'Result >= Value else Inflate_Finite'Result <= Value);
   pragma Inline (Inflate_Finite);
   --  Callers establish |Value| <= Last/2. Two model epsilons (or Model_Small near zero) exceed one ulp,
   --  so this bounded kernel needs neither an overflow check nor an Adjacent fallback.

   function Inflate_Finite (Value : Dimensionless; Upward : Boolean) return Dimensionless is
      pragma Suppress (Overflow_Check);
      --  Roundoff_Padding's formula without its overflow check. Not 'Max: GNAT guards that with a NaN test, which
      --  measurably slows the interval kernels this is inlined into. Both forms give Model_Small for NaN.
      Relative : constant Dimensionless := 2.0 * Dimensionless'Model_Epsilon * abs Value;
      Padding  : constant Dimensionless :=
        (if Relative > Dimensionless'Model_Small then Relative else Dimensionless'Model_Small);
   begin
      return (if Upward then Value + Padding else Value - Padding);
   end Inflate_Finite;

   procedure Inflate_Finite_Endpoints (Lower, Upper : in out Dimensionless)
   with
     Pre  => abs Lower <= Dimensionless'Last / 2.0 and then abs Upper <= Dimensionless'Last / 2.0,
     Post => Lower <= Lower'Old and then Upper >= Upper'Old;
   pragma Inline (Inflate_Finite_Endpoints);
   --  Callers must prove that both endpoints are finite and have magnitude below Last/2. The same padding used by
   --  Down/Up then cannot overflow and exceeds one ulp, so neither saturation nor the Adjacent fallback is needed.

   procedure Inflate_Finite_Endpoints (Lower, Upper : in out Dimensionless) is
   begin
      Lower := Inflate_Finite (Lower, False);
      Upper := Inflate_Finite (Upper, True);
   end Inflate_Finite_Endpoints;

   Exact_Binomial_Bound_Limit : constant := 2.0**130;
   --  Above the outward-rounded conversion of every 128-bit integer.

   --  Enclose the Bernstein product or degree-elevation weight
   --
   --     C(Left_N, Left_K) * C(Right_N, Right_K) / C(Denominator_N, Denominator_K).
   --
   --  The integer products are exact; only their final conversion and division need outward rounding.
   function Bernstein_Product_Weight
     (Left_N, Left_K, Right_N, Right_K, Denominator_N, Denominator_K : Natural) return Interval
   with Post => Stereographic_Kernel.Intervals.Is_Ordered (Bernstein_Product_Weight'Result);
   --  Enclose a ratio of exact binomial-coefficient products.

   function Bernstein_Product_Weight
     (Left_N, Left_K, Right_N, Right_K, Denominator_N, Denominator_K : Natural) return Interval
   is
      use type Interfaces.Unsigned_128;
      Numerator   : constant Exact_Binomial_Value :=
        Exact_Binomial (Left_N, Left_K) * Exact_Binomial (Right_N, Right_K);
      Denominator : constant Exact_Binomial_Value := Exact_Binomial (Denominator_N, Denominator_K);
   begin
      if Denominator = 0 then
         return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      elsif Numerator = 0 then
         return Interval_Exact (0.0);
      elsif Numerator = Denominator then
         return Interval_Exact (1.0);
      end if;
      declare
         Numerator_Centre   : constant Dimensionless := To_Dimensionless (Numerator);
         Denominator_Centre : constant Dimensionless := To_Dimensionless (Denominator);
         Numerator_Lower    : constant Dimensionless := Down (Numerator_Centre);
         Numerator_Upper    : constant Dimensionless := Up (Numerator_Centre);
         Denominator_Lower  : constant Dimensionless := Down (Denominator_Centre);
         Denominator_Upper  : constant Dimensionless := Up (Denominator_Centre);
      begin
         --  Both integers are at least one and below 2**128, so these tests never fail. They state the size of
         --  the operands of the two quotients.
         if not (Numerator_Lower >= 0.0
                 and then Numerator_Upper <= Exact_Binomial_Bound_Limit
                 and then Denominator_Lower >= 0.5
                 and then Denominator_Upper <= Exact_Binomial_Bound_Limit)
         then
            return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
         end if;
         --  Both bounds are strictly positive. Specializing their quotient avoids constructing a reciprocal and
         --  feeding it through the generic four-product interval multiplier for every Bernstein product weight.
         return
           Checked_Interval (Down (Numerator_Lower / Denominator_Upper), Up (Numerator_Upper / Denominator_Lower));
      end;
   end Bernstein_Product_Weight;

   type Positive_Endpoints is record
      Lower, Upper : Dimensionless;
   end record;
   --  The cached scales are finite and strictly positive, so no per-entry validity flag is needed.

   type Binomial_Scale is record
      Factor, Reciprocal : Positive_Endpoints;
   end record;
   type Binomial_Scale_Array is array (Packed_Binomial_Index) of Binomial_Scale;

   function Is_Positive_Scale (Scale : Binomial_Scale) return Boolean
   is (Scale.Factor.Lower >= 0.5
       and then Scale.Factor.Lower <= Scale.Factor.Upper
       and then Scale.Factor.Upper <= Exact_Binomial_Bound_Limit
       and then Scale.Reciprocal.Lower >= 2.0**(-132)
       and then Scale.Reciprocal.Upper >= Scale.Reciprocal.Lower
       and then Scale.Reciprocal.Upper <= 4.0);
   --  Whether both enclosures are of a positive integer below 2**128 and of its reciprocal.

   function Scale_Of (Value : Exact_Binomial_Value) return Binomial_Scale
   with Post => Is_Positive_Scale (Scale_Of'Result);
   --  Enclosures of a binomial coefficient and of its reciprocal.

   function Scale_Of (Value : Exact_Binomial_Value) return Binomial_Scale is
      use type Interfaces.Unsigned_128;
      Centre : constant Dimensionless := Dimensionless'Max (1.0, To_Dimensionless (Value));
      --  A binomial coefficient is at least one, so the maximum never changes it. Nor does the one below:
      --  rounding one or more downward leaves more than a half.
      Factor : constant Positive_Endpoints :=
        (if Value = 1
         then (Lower => 1.0, Upper => 1.0)
         elsif Value <= 2**Dimensionless'Machine_Mantissa
         then (Lower => Centre, Upper => Centre)
         else (Lower => Dimensionless'Max (0.5, Down (Centre)), Upper => Up (Centre)));
   begin
      pragma Assert (Centre >= 1.0 and then Centre <= 2.0**128);
      pragma
        Assert
          (Factor.Lower >= 0.5
           and then Factor.Lower <= Factor.Upper
           and then Factor.Upper <= Exact_Binomial_Bound_Limit);
      if Value = 1 then
         return (Factor => Factor, Reciprocal => Factor);
      end if;
      declare
         Lower_Quotient   : constant Dimensionless := 1.0 / Factor.Upper;
         Upper_Quotient   : constant Dimensionless := 1.0 / Factor.Lower;
         Reciprocal_Lower : constant Dimensionless := Down (Lower_Quotient);
         Reciprocal_Upper : constant Dimensionless := Up (Upper_Quotient);
      begin
         pragma Assert (Lower_Quotient >= 2.0**(-130) and then Lower_Quotient <= Upper_Quotient);
         pragma Assert (Upper_Quotient <= 2.0);
         pragma Assert (Reciprocal_Lower >= 2.0**(-132));
         pragma Assert (Reciprocal_Upper <= 4.0);
         pragma Assert (Reciprocal_Lower <= Reciprocal_Upper);
         return (Factor => Factor, Reciprocal => (Lower => Reciprocal_Lower, Upper => Reciprocal_Upper));
      end;
   end Scale_Of;

   function Build_Binomial_Scales return Binomial_Scale_Array
   with Post => (for all Index in Packed_Binomial_Index => Is_Positive_Scale (Build_Binomial_Scales'Result (Index)));
   --  Enclose the exact integer scales and their reciprocals once during package elaboration.

   function Build_Binomial_Scales return Binomial_Scale_Array is
      Result : Binomial_Scale_Array :=
        [others => (Factor => (Lower => 1.0, Upper => 1.0), Reciprocal => (Lower => 1.0, Upper => 1.0))];
      --  Only the entries with K <= N / 2 are assigned below and read afterwards.
   begin
      for N in 0 .. Maximum_Derivative_Bernstein_Degree loop
         pragma Loop_Invariant (for all Index in Packed_Binomial_Index => Is_Positive_Scale (Result (Index)));
         for K in 0 .. N / 2 loop
            pragma Loop_Invariant (for all Index in Packed_Binomial_Index => Is_Positive_Scale (Result (Index)));
            Result (Binomial_Index (N, K)) := Scale_Of (Exact_Binomial (N, K));
         end loop;
      end loop;
      return Result;
   end Build_Binomial_Scales;

   Binomial_Scales : constant Binomial_Scale_Array := Build_Binomial_Scales;
   --  These positive enclosures depend only on degree and index. Build them once, including their outward-rounded
   --  reciprocals, instead of repeatedly converting 128-bit integers and dividing during every polynomial product.

   function Multiply_Bernstein (Left, Right : Interval_Polynomial) return Interval_Polynomial is
      Degree          : constant Natural := Left'Last + Right'Last;
      Scaled_Left     : Interval_Polynomial (Left'Range);
      Scaled_Right    : Interval_Polynomial (Right'Range);
      Result          : Interval_Polynomial (0 .. Degree) := [others => Interval_Exact (0.0)];
      Left_Max        : Dimensionless := 0.0;
      Right_Max       : Dimensionless := 0.0;
      Invalid_Product : constant Interval :=
        (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);

      function Usable (Value : Interval) return Boolean
      is (Value.Valid
          and then Value.Lower > -Dimensionless'Last
          and then Value.Upper < Dimensionless'Last
          and then Value.Lower <= Value.Upper);
      --  Saturated endpoints must trigger fallback before a later scale or cancellation could make them finite.

      function Scale_By_Positive (Value : Interval; Factor : Positive_Endpoints) return Interval;
      pragma Inline (Scale_By_Positive);
      --  Binomial scales and their reciprocals are valid and strictly positive. Each endpoint therefore chooses its
      --  factor independently by sign; the generic multiplier's other sign combinations cannot occur here.

      function Scale_By_Positive (Value : Interval; Factor : Positive_Endpoints) return Interval is
         Lower, Upper : Dimensionless;
      begin
         if not Value.Valid or else (Value.Lower = 0.0 and then Value.Upper = 0.0) then
            return Value;
         elsif Factor.Lower = 1.0 and then Factor.Upper = 1.0 then
            if Usable (Value) then
               return Value;
            else
               return Invalid_Product;
            end if;
         elsif Value.Lower = 1.0 and then Value.Upper = 1.0 then
            return (Lower => Factor.Lower, Upper => Factor.Upper, Valid => True);
         elsif Value.Lower = -1.0 and then Value.Upper = -1.0 then
            return (Lower => -Factor.Upper, Upper => -Factor.Lower, Valid => True);
         end if;
         Lower := Value.Lower * (if Value.Lower < 0.0 then Factor.Upper else Factor.Lower);
         Upper := Value.Upper * (if Value.Upper < 0.0 then Factor.Lower else Factor.Upper);
         if Lower > -Dimensionless'Last / 2.0 and then Upper < Dimensionless'Last / 2.0 and then Lower <= Upper then
            Inflate_Finite_Endpoints (Lower, Upper);
            return (Lower => Lower, Upper => Upper, Valid => True);
         else
            declare
               Product : constant Interval :=
                 Interval_Multiply (Value, (Lower => Factor.Lower, Upper => Factor.Upper, Valid => True));
            begin
               if Usable (Product) then
                  return Product;
               else
                  return Invalid_Product;
               end if;
            end;
         end if;
      end Scale_By_Positive;

      function Scale_Bounded_By_Reciprocal (Value : Interval; Factor : Positive_Endpoints) return Interval;
      pragma Inline (Scale_Bounded_By_Reciprocal);

      function Scale_Bounded_By_Reciprocal (Value : Interval; Factor : Positive_Endpoints) return Interval is
         pragma Suppress (Overflow_Check);
         Lower, Upper : Dimensionless;
         --  The convolution preflight and outward padding leave every result below Last/2 in magnitude.
         --  Binomial reciprocals lie in (0, 1], so this final scaling cannot overflow or invalidate an interval.
      begin
         if (Factor.Lower = 1.0 and then Factor.Upper = 1.0) or else (Value.Lower = 0.0 and then Value.Upper = 0.0)
         then
            return Value;
         elsif Value.Lower = 1.0 and then Value.Upper = 1.0 then
            return (Lower => Factor.Lower, Upper => Factor.Upper, Valid => True);
         elsif Value.Lower = -1.0 and then Value.Upper = -1.0 then
            return (Lower => -Factor.Upper, Upper => -Factor.Lower, Valid => True);
         end if;
         Lower := Value.Lower * (if Value.Lower < 0.0 then Factor.Upper else Factor.Lower);
         Upper := Value.Upper * (if Value.Upper < 0.0 then Factor.Lower else Factor.Upper);
         Inflate_Finite_Endpoints (Lower, Upper);
         return (Lower => Lower, Upper => Upper, Valid => True);
      end Scale_Bounded_By_Reciprocal;

      procedure Accumulate_Product (Sum : in out Interval; Left, Right : Interval);
      pragma Inline (Accumulate_Product);
      --  Both operands and the accumulator are valid, and the convolution preflight proves that every product and
      --  partial sum stays finite. Retain ordinary interval rounding and exact identities without rechecking those
      --  invariants or constructing temporary interval records at each term.

      procedure Accumulate_Product (Sum : in out Interval; Left, Right : Interval) is
         pragma Suppress (Overflow_Check);
         Lower_Product, Upper_Product : Dimensionless;

         procedure Bounded_Product_Extrema (Left, Right : Interval; Lower_Product, Upper_Product : out Dimensionless);
         pragma Inline (Bounded_Product_Extrema);
         --  The convolution preflight bounds every corner product, including products not selected as extrema.

         procedure Bounded_Product_Extrema (Left, Right : Interval; Lower_Product, Upper_Product : out Dimensionless)
         is
         begin
            if Left.Lower >= 0.0 then
               if Right.Lower >= 0.0 then
                  Lower_Product := Left.Lower * Right.Lower;
                  Upper_Product := Left.Upper * Right.Upper;
               elsif Right.Upper <= 0.0 then
                  Lower_Product := Left.Upper * Right.Lower;
                  Upper_Product := Left.Lower * Right.Upper;
               else
                  Lower_Product := Left.Upper * Right.Lower;
                  Upper_Product := Left.Upper * Right.Upper;
               end if;
            elsif Left.Upper <= 0.0 then
               if Right.Lower >= 0.0 then
                  Lower_Product := Left.Lower * Right.Upper;
                  Upper_Product := Left.Upper * Right.Lower;
               elsif Right.Upper <= 0.0 then
                  Lower_Product := Left.Upper * Right.Upper;
                  Upper_Product := Left.Lower * Right.Lower;
               else
                  Lower_Product := Left.Lower * Right.Upper;
                  Upper_Product := Left.Lower * Right.Lower;
               end if;
            elsif Right.Lower >= 0.0 then
               Lower_Product := Left.Lower * Right.Upper;
               Upper_Product := Left.Upper * Right.Upper;
            elsif Right.Upper <= 0.0 then
               Lower_Product := Left.Upper * Right.Lower;
               Upper_Product := Left.Lower * Right.Lower;
            else
               Lower_Product := Dimensionless'Min (Left.Lower * Right.Upper, Left.Upper * Right.Lower);
               Upper_Product := Dimensionless'Max (Left.Lower * Right.Lower, Left.Upper * Right.Upper);
            end if;
         end Bounded_Product_Extrema;
      begin
         --  The outer loop already skips exact-zero left controls.
         if Right.Lower = 0.0 and then Right.Upper = 0.0 then
            return;
         elsif Left.Lower = 1.0 and then Left.Upper = 1.0 then
            Lower_Product := Right.Lower;
            Upper_Product := Right.Upper;
         elsif Right.Lower = 1.0 and then Right.Upper = 1.0 then
            Lower_Product := Left.Lower;
            Upper_Product := Left.Upper;
         elsif Left.Lower = -1.0 and then Left.Upper = -1.0 then
            Lower_Product := -Right.Upper;
            Upper_Product := -Right.Lower;
         elsif Right.Lower = -1.0 and then Right.Upper = -1.0 then
            Lower_Product := -Left.Upper;
            Upper_Product := -Left.Lower;
         else
            Bounded_Product_Extrema (Left, Right, Lower_Product, Upper_Product);
            Inflate_Finite_Endpoints (Lower_Product, Upper_Product);
         end if;
         if Sum.Lower = 0.0 and then Sum.Upper = 0.0 then
            Sum.Lower := Lower_Product;
            Sum.Upper := Upper_Product;
         else
            Sum.Lower := Sum.Lower + Lower_Product;
            Sum.Upper := Sum.Upper + Upper_Product;
            Inflate_Finite_Endpoints (Sum.Lower, Sum.Upper);
         end if;
      end Accumulate_Product;

      function Unscaled_Product return Interval_Polynomial;
      --  Convolve with an individual enclosing weight per term. This handles a degree-zero operand and is the fallback
      --  when the faster intermediate scaling overflows.

      function Unscaled_Product return Interval_Polynomial is
         Product : Interval_Polynomial (0 .. Degree) := [others => Interval_Exact (0.0)];
      begin
         for I in Left'Range loop
            for J in Right'Range loop
               declare
                  Weight : constant Interval := Bernstein_Product_Weight (Left'Last, I, Right'Last, J, Degree, I + J);
                  Term   : Interval := Interval_Multiply (Left (I), Right (J));
               begin
                  if not Usable (Term) then
                     return
                       [0 .. Degree => (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False)];
                  end if;
                  Term := Interval_Multiply (Weight, Term);
                  if not Usable (Term) then
                     return
                       [0 .. Degree => (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False)];
                  end if;
                  Product (I + J) := Interval_Add (Product (I + J), Term);
                  if not Usable (Product (I + J)) then
                     return
                       [0 .. Degree => (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False)];
                  end if;
               end;
            end loop;
         end loop;
         return Product;
      end Unscaled_Product;
   begin
      --  C(m+n,k) * (Left*Right)_k = sum_(i+j=k) (C(m,i)*Left_i) * (C(n,j)*Right_j).
      --  All scales are positive. This retains Bernstein form without an alternating power-basis conversion.
      --  Enclose each scale, product, sum and final quotient independently. Scaling costs O(m+n), and the quadratic
      --  convolution needs only one interval multiplication per term instead of a product and a separate weight.
      if Left'Last = 0 or else Right'Last = 0 then
         return Unscaled_Product;
      end if;
      for I in Left'Range loop
         Scaled_Left (I) := Scale_By_Positive (Left (I), Binomial_Scales (Binomial_Index (Left'Last, I)).Factor);
         if not Scaled_Left (I).Valid then
            return Unscaled_Product;
         end if;
         Left_Max := Dimensionless'Max (Left_Max, Interval_Abs_Max (Scaled_Left (I)));
      end loop;
      for J in Right'Range loop
         Scaled_Right (J) := Scale_By_Positive (Right (J), Binomial_Scales (Binomial_Index (Right'Last, J)).Factor);
         if not Scaled_Right (J).Valid then
            return Unscaled_Product;
         end if;
         Right_Max := Dimensionless'Max (Right_Max, Interval_Abs_Max (Scaled_Right (J)));
      end loop;
      --  Both operands have been validated before skipping zero terms. In particular, zero must never conceal an
      --  invalid coefficient in the other operand. The maxima also identify all-zero polynomials without rescanning.
      if Left_Max = 0.0 or else Right_Max = 0.0 then
         return Result;
      end if;
      declare
         Larger  : constant Dimensionless := Dimensionless'Max (Left_Max, Right_Max);
         Smaller : constant Dimensionless := Dimensionless'Min (Left_Max, Right_Max);
         Limit   : constant Dimensionless :=
           (Dimensionless'Last / 4.0) / Dimensionless (Natural'Min (Left'Length, Right'Length));
      begin
         --  At most min(m+1,n+1) terms contribute to one control. Reserve a factor of four for outward rounding so
         --  no product or partial sum can overflow, even before cancellation. This also covers release builds where
         --  floating overflow need not raise: a saturated intermediate must never be divided back into a bound.
         if Larger > 1.0 and then Smaller > Limit / Larger then
            return Unscaled_Product;
         end if;
      end;
      declare
         pragma Suppress (Index_Check);
         pragma Suppress (Range_Check);
         pragma Suppress (Overflow_Check);
         --  I and J stay in the corresponding operand ranges; 0 <= I+J <= Degree, whose checked sum was
         --  computed above. The finite-product preflight also bounds every intermediate of Accumulate_Product.
      begin
         for I in Left'Range loop
            if Scaled_Left (I).Lower /= 0.0 or else Scaled_Left (I).Upper /= 0.0 then
               for J in Right'Range loop
                  Accumulate_Product (Result (I + J), Scaled_Left (I), Scaled_Right (J));
               end loop;
            end if;
         end loop;
      end;
      for K in Result'Range loop
         Result (K) :=
           Scale_Bounded_By_Reciprocal (Result (K), Binomial_Scales (Binomial_Index (Degree, K)).Reciprocal);
      end loop;
      return Result;
   exception
      when Constraint_Error =>
         return Unscaled_Product;
   end Multiply_Bernstein;

   function Frame_Speed_Upper_Bound (Frame : Frame_Vector_Array) return Dimensionless
   with Post => Frame_Speed_Upper_Bound'Result >= 0.0;
   --  Bound the operator norm of a floating-point tangent frame from an outward-rounded Gram matrix.

   function Frame_Speed_Upper_Bound (Frame : Frame_Vector_Array) return Dimensionless is
      Maximum_Row_Sum : Dimensionless := 0.0;
   begin
      for I in Frame_Component_Index loop
         declare
            Row_Sum : Interval := Interval_Exact (0.0);
         begin
            for J in Frame_Component_Index loop
               declare
                  Dot : Interval := Interval_Exact (0.0);
               begin
                  for Axis in Axis_Name loop
                     Dot :=
                       Interval_Add
                         (Dot,
                          Interval_Multiply (Interval_Exact (Frame (I) (Axis)), Interval_Exact (Frame (J) (Axis))));
                  end loop;
                  if not Dot.Valid then
                     return Dimensionless'Last;
                  end if;
                  Row_Sum := Interval_Add (Row_Sum, Interval_Exact (Interval_Abs_Max (Dot)));
               end;
            end loop;
            if not Row_Sum.Valid then
               return Dimensionless'Last;
            end if;
            Maximum_Row_Sum := Dimensionless'Max (Maximum_Row_Sum, Row_Sum.Upper);
         end;
      end loop;
      return Certified_Upper_Square_Root (Dimensionless'Max (0.0, Maximum_Row_Sum));
   end Frame_Speed_Upper_Bound;

   function Finish_Correction_Interval (Evaluator : Stereographic_Curve_Evaluator; Axis : Axis_Name) return Interval
   is (Stereographic_Kernel.Finish_Correction.Correction_Interval
         (Dimensionless (Evaluator.Finish_Point (Axis) / mm),
          Dimensionless (Evaluator.Uncorrected_Finish_Point (Axis) / mm)));

   function Finish_Correction_Position_Error_Bound (Evaluator : Stereographic_Curve_Evaluator) return Dimensionless
   with Post => Finish_Correction_Position_Error_Bound'Result >= 0.0;
   --  Bound the Euclidean endpoint-correction displacement in millimetres without a rounded vector subtraction.

   function Finish_Correction_Position_Error_Bound (Evaluator : Stereographic_Curve_Evaluator) return Dimensionless is
      Axis_Bounds : Dimensionless_Axis_Vector := [others => 0.0];
      Scale       : Dimensionless := 0.0;
      Square_Sum  : Dimensionless := 0.0;
   begin
      --  The smootherstep lies in [0, 1], so the Euclidean norm of Finish_Point - Uncorrected_Finish_Point bounds
      --  its uniform position contribution. Form every subtraction outward and scale the norm to avoid overflow.
      for Axis in Axis_Name loop
         pragma
           Loop_Invariant
             (Scale >= 0.0
              and then (for all B in Axis_Name => Axis_Bounds (B) >= 0.0 and then Axis_Bounds (B) <= Scale));
         declare
            Correction : constant Interval := Finish_Correction_Interval (Evaluator, Axis);
            Bound      : Dimensionless;
         begin
            if not Correction.Valid then
               return Dimensionless'Last;
            end if;
            Bound := Interval_Abs_Max (Correction);
            Axis_Bounds (Axis) := Bound;
            Scale := Dimensionless'Max (Scale, Bound);
         end;
      end loop;
      pragma Assert (for all B in Axis_Name => Axis_Bounds (B) >= 0.0 and then Axis_Bounds (B) <= Scale);
      if Scale = 0.0 then
         return 0.0;
      elsif not (Scale <= Tame_Limit) then
         --  A correction beyond the common limit has no bound worth forming.
         return Dimensionless'Last;
      end if;
      for Axis in Axis_Name loop
         pragma Loop_Invariant (Square_Sum >= 0.0 and then Square_Sum <= Tame_Limit);
         pragma Assert (Axis_Bounds (Axis) >= 0.0 and then Axis_Bounds (Axis) <= Scale);
         declare
            --  Each ratio is at most one, up to the outward rounding, so the sum of the squares is at most a
            --  little over four. Anything else would be no bound.
            Ratio : constant Dimensionless := Up (Axis_Bounds (Axis) / Scale);
         begin
            if not (Ratio <= 2.0) then
               return Dimensionless'Last;
            end if;
            Square_Sum := Up (Square_Sum + Up (Ratio * Ratio));
            if not (Square_Sum <= Tame_Limit) then
               return Dimensionless'Last;
            end if;
         end;
      end loop;
      declare
         Unit_Norm_Bound : constant Dimensionless := Certified_Upper_Square_Root (Square_Sum);
      begin
         if not (Unit_Norm_Bound <= Tame_Limit) then
            return Dimensionless'Last;
         else
            return Up (Scale * Unit_Norm_Bound);
         end if;
      end;
   end Finish_Correction_Position_Error_Bound;

   function Is_Zero_Projection (Coefficients : Projection_Coefficients) return Boolean is
   begin
      for A in Axis_Name loop
         if Coefficients (A) /= 0.0 / mm then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zero_Projection;

   ----------------------------------------------------------------------------------------------------------------
   --  Closure construction. A bounded direct four-variable solve determines
   --  the three closure-mode amplitudes and the curve length together.
   ----------------------------------------------------------------------------------------------------------------

   Closure_Envelope_Power : constant array (Natural range 4 .. 8) of Dimensionless := [1.0, -4.0, 6.0, -4.0, 1.0];
   --  V⁴·(1 - V)⁴ in the power basis.

   procedure Lemma_Power_Chart_Within_Larger (Value : Power_Chart; Smaller, Larger : Dimensionless)
   with Ghost, Pre => Within (Value, Smaller) and then Smaller <= Larger, Post => Within (Value, Larger);

   procedure Lemma_Power_Chart_Within_Larger (Value : Power_Chart; Smaller, Larger : Dimensionless) is null;

   function Single_Order_Jet (Order : Natural; Value : Chart_Vector) return Chart_Jet_Array
   with Pre => Order <= 3 and then Within (Value, Tame_Limit), Post => Within (Single_Order_Jet'Result, Tame_Limit);
   --  The jet whose only coefficient is the given one.

   function Single_Order_Jet (Order : Natural; Value : Chart_Vector) return Chart_Jet_Array is
      Zero : constant Chart_Vector := [others => 0.0];
   begin
      --  Written out for each order: GNAT 16.2 fails an internal assertion on an iterated component association
      --  whose value is a conditional expression with an aggregate in it.
      return
        [0 => (if Order = 0 then Value else Zero),
         1 => (if Order = 1 then Value else Zero),
         2 => (if Order = 2 then Value else Zero),
         3 => (if Order = 3 then Value else Zero)];
   end Single_Order_Jet;

   function Without_Top_Residue (Chart : Power_Chart) return Power_Chart
   with Pre => Within (Chart, Power_Basis_Limit), Post => Within (Without_Top_Residue'Result, Lambda_Terms_Limit);
   --  The Hermite part of a chart has degree seven. Remove the rounding residue in its V⁸ coefficient as a multiple
   --  of the whole closure envelope, which leaves both endpoint jets untouched.

   function Without_Top_Residue (Chart : Power_Chart) return Power_Chart is
      Result : Power_Chart := Chart;
   begin
      Lemma_Power_Chart_Within_Larger (Result, Power_Basis_Limit, Lambda_Terms_Limit);
      for C in Chart_Component_Index loop
         pragma Loop_Invariant (Within (Result, Lambda_Terms_Limit));
         pragma Assert (abs Chart (Fixed_Chart_Degree, C) <= Power_Basis_Limit);
         declare
            Correction : constant Dimensionless := -Chart (Fixed_Chart_Degree, C);
         begin
            for Degree in Closure_Envelope_Power'Range loop
               pragma Loop_Invariant (Within (Result, Lambda_Terms_Limit));
               pragma Assert (abs Chart (Degree, C) <= Power_Basis_Limit);
               pragma Assert (abs Closure_Envelope_Power (Degree) <= 6.0);
               declare
                  Value : constant Dimensionless := Chart (Degree, C) + Correction * Closure_Envelope_Power (Degree);
               begin
                  pragma Assert (abs Value <= Lambda_Terms_Limit);
                  Set_Power_Coefficient (Result, Degree, C, Value, Lambda_Terms_Limit);
               end;
            end loop;
         end;
      end loop;
      return Result;
   end Without_Top_Residue;

   function Lambda_Term
     (Start_Jets, Finish_Jets : Chart_Jet_Array; Order : Natural; Warp_Factor : Dimensionless) return Power_Chart
   with
     Pre  =>
       Order <= 3
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Within (Start_Jets, Tame_Limit)
       and then Within (Finish_Jets, Tame_Limit),
     Post => Within (Lambda_Term'Result, Lambda_Terms_Limit);
   --  The power coefficients of the chart which multiplies the given power of the length ratio.

   function Lambda_Term
     (Start_Jets, Finish_Jets : Chart_Jet_Array; Order : Natural; Warp_Factor : Dimensionless) return Power_Chart
   is
      Zero  : constant Raw_Vector_3 := [others => 0.0];
      Chart : Bernstein_Chart;
   begin
      pragma Assert (Within (Start_Jets (Order), Tame_Limit) and then Within (Finish_Jets (Order), Tame_Limit));
      pragma Assert (Within (Zero, Tame_Limit));
      Build_Chart
        (Single_Order_Jet (Order, Start_Jets (Order)),
         Single_Order_Jet (Order, Finish_Jets (Order)),
         1.0,
         Zero,
         Warp_Factor,
         Chart);
      return Without_Top_Residue (Power_Basis (Chart));
   end Lambda_Term;

   function Build_Chart_Lambda_Terms
     (Start_Jets, Finish_Jets : Chart_Jet_Array; Warp_Factor : Dimensionless) return Chart_Lambda_Terms
   is
      T0 : constant Power_Chart := Lambda_Term (Start_Jets, Finish_Jets, 0, Warp_Factor);
      T1 : constant Power_Chart := Lambda_Term (Start_Jets, Finish_Jets, 1, Warp_Factor);
      T2 : constant Power_Chart := Lambda_Term (Start_Jets, Finish_Jets, 2, Warp_Factor);
      T3 : constant Power_Chart := Lambda_Term (Start_Jets, Finish_Jets, 3, Warp_Factor);
   begin
      return [0 => T0, 1 => T1, 2 => T2, 3 => T3];
   end Build_Chart_Lambda_Terms;

   function Lambda_Coefficient (T0, T1, T2, T3, Lambda : Dimensionless) return Dimensionless
   is (T0 + Lambda * (T1 + Lambda * (T2 + Lambda * T3)))
   with
     Pre  =>
       abs T0 <= Tame_Limit
       and then abs T1 <= Tame_Limit
       and then abs T2 <= Tame_Limit
       and then abs T3 <= Tame_Limit
       and then abs Lambda <= Lambda_Limit,
     Post => abs Lambda_Coefficient'Result <= 2.0**223;
   --  A coefficient of the chart as a cubic in the length ratio.

   function D_Lambda_Coefficient (T1, T2, T3, Lambda : Dimensionless) return Dimensionless
   is (T1 + Lambda * (2.0 * T2 + 3.0 * Lambda * T3))
   with
     Pre  =>
       abs T1 <= Tame_Limit
       and then abs T2 <= Tame_Limit
       and then abs T3 <= Tame_Limit
       and then abs Lambda <= Lambda_Limit,
     Post => abs D_Lambda_Coefficient'Result <= 2.0**223;
   --  The derivative of that cubic with respect to the length ratio.

   procedure Chart_At_Lambda
     (Terms                               : Chart_Lambda_Terms;
      Lambda                              : Dimensionless;
      C0                                  : Raw_Vector_3;
      Coefficients, D_Lambda_Coefficients : out Power_Chart) is
   begin
      Coefficients := [others => [others => 0.0]];
      D_Lambda_Coefficients := [others => [others => 0.0]];
      pragma Assert (Within (Coefficients, Closure_Chart_Limit));
      pragma Assert (Within (D_Lambda_Coefficients, Closure_Chart_Limit));
      for Degree in Chart_Coefficient_Index loop
         pragma Loop_Invariant (Within (Coefficients, Closure_Chart_Limit));
         pragma Loop_Invariant (Within (D_Lambda_Coefficients, Closure_Chart_Limit));
         for C in Chart_Component_Index loop
            pragma Loop_Invariant (Within (Coefficients, Closure_Chart_Limit));
            pragma Loop_Invariant (Within (D_Lambda_Coefficients, Closure_Chart_Limit));
            pragma
              Assert
                (abs Terms (0) (Degree, C) <= Tame_Limit
                 and then abs Terms (1) (Degree, C) <= Tame_Limit
                 and then abs Terms (2) (Degree, C) <= Tame_Limit
                 and then abs Terms (3) (Degree, C) <= Tame_Limit);
            pragma Assert (abs C0 (C) <= Tame_Limit);
            declare
               Cubic   : constant Dimensionless :=
                 Lambda_Coefficient
                   (Terms (0) (Degree, C),
                    Terms (1) (Degree, C),
                    Terms (2) (Degree, C),
                    Terms (3) (Degree, C),
                    Lambda);
               D_Cubic : constant Dimensionless :=
                 D_Lambda_Coefficient (Terms (1) (Degree, C), Terms (2) (Degree, C), Terms (3) (Degree, C), Lambda);
            begin
               Set_Power_Coefficient (D_Lambda_Coefficients, Degree, C, D_Cubic, Closure_Chart_Limit);
               if Degree in Closure_Envelope_Power'Range then
                  --  Add C₀·256·V⁴·(1 - V)⁴ as a whole. Its fourfold zeros leave every endpoint jet through order
                  --  three unchanged, which changing only the V⁸ coefficient would not.
                  pragma Assert (abs Closure_Envelope_Power (Degree) <= 6.0);
                  declare
                     Closed : constant Dimensionless := Cubic + (256.0 * C0 (C)) * Closure_Envelope_Power (Degree);
                  begin
                     pragma Assert (abs Closed <= Closure_Chart_Limit);
                     Set_Power_Coefficient (Coefficients, Degree, C, Closed, Closure_Chart_Limit);
                  end;
               else
                  Set_Power_Coefficient (Coefficients, Degree, C, Cubic, Closure_Chart_Limit);
               end if;
            end;
         end loop;
      end loop;
   end Chart_At_Lambda;

   Closure_Point_Limit : constant := 2.0**228;
   --  Above a point of the chart of a closure residual, and of its derivative with respect to the length ratio:
   --  nine power coefficients within Closure_Chart_Limit at a parameter between zero and one.

   Node_Limit : constant := 2.0**690;
   --  Above everything one quadrature node adds to a sum of the closure residual. The largest is a derivative of
   --  the tangent with respect to the length ratio, three products of a Jacobian entry below 2⁴⁵⁹ and a chart
   --  derivative. These bounds are far above the true magnitudes, which are of order one, because they ignore
   --  that the sphere map divides by what it multiplies.

   Maximum_Nodes : constant := 16 * Maximum_Panel_Count;

   function Horner_Value
     (Chart : Power_Chart; Component : Chart_Component_Index; V : Dimensionless) return Dimensionless
   with
     Pre  => Within (Chart, Closure_Chart_Limit) and then V >= 0.0 and then V <= 1.0,
     Post => abs Horner_Value'Result <= Closure_Point_Limit;
   --  Horner evaluation uses one multiply and one add per power coefficient:
   --
   --     Y꜀(V) = (…((a₈·V + a₇)·V + a₆)…)·V + a₀.

   function Horner_Value
     (Chart : Power_Chart; Component : Chart_Component_Index; V : Dimensionless) return Dimensionless
   is
      pragma Suppress (Overflow_Check);
      --  This and the steps below are the innermost loop of the closure solve. They are proved not to overflow,
      --  so the unit's floating-point overflow checks are left out of them.
      Y : Dimensionless := 0.0;
   begin
      for Degree in reverse 0 .. Fixed_Chart_Degree loop
         pragma Loop_Invariant (abs Y <= Dimensionless (Fixed_Chart_Degree - Degree) * Closure_Chart_Limit);
         pragma Assert (abs Chart (Degree, Component) <= Closure_Chart_Limit);
         declare
            Next : constant Dimensionless := Y * V + Chart (Degree, Component);
         begin
            pragma Assert (abs Next <= Dimensionless (Fixed_Chart_Degree - Degree + 1) * Closure_Chart_Limit);
            Y := Next;
         end;
      end loop;
      pragma Assert (abs Y <= 9.0 * Closure_Chart_Limit);
      return Y;
   end Horner_Value;
   pragma Inline (Horner_Value);

   function Closure_Point (Y1, Y2, Y3 : Dimensionless) return Chart_Vector
   is ([1 => Y1, 2 => Y2, 3 => Y3])
   with
     Pre  =>
       abs Y1 <= Closure_Point_Limit and then abs Y2 <= Closure_Point_Limit and then abs Y3 <= Closure_Point_Limit,
     Post => Within (Closure_Point'Result, Closure_Point_Limit);

   function Node_Vector (A, B, C, D : Dimensionless) return Raw_Vector_4
   is ([0 => A, 1 => B, 2 => C, 3 => D])
   with
     Pre  =>
       abs A <= Node_Limit and then abs B <= Node_Limit and then abs C <= Node_Limit and then abs D <= Node_Limit,
     Post => Within (Node_Vector'Result, Node_Limit);

   procedure Sphere_Row
     (Y_Row, Y1, Y2, Y3, D1, D2, D3 : Dimensionless;
      Row                           : Chart_Component_Index;
      Inverse, Quartic, Envelope    : Dimensionless;
      T, D_T, JE_1, JE_2, JE_3      : out Dimensionless)
   with
     Pre  =>
       abs Y_Row <= Closure_Point_Limit
       and then abs Y1 <= Closure_Point_Limit
       and then abs Y2 <= Closure_Point_Limit
       and then abs Y3 <= Closure_Point_Limit
       and then abs D1 <= Closure_Point_Limit
       and then abs D2 <= Closure_Point_Limit
       and then abs D3 <= Closure_Point_Limit
       and then Inverse >= 0.0
       and then Inverse <= 1.0
       and then Quartic >= 0.0
       and then Quartic <= 4.0
       and then Envelope >= 0.0
       and then Envelope <= 256.0,
     Post =>
       abs T <= Node_Limit
       and then abs D_T <= Node_Limit
       and then abs JE_1 <= Node_Limit
       and then abs JE_2 <= Node_Limit
       and then abs JE_3 <= Node_Limit;
   --  One of the last three components of S(Y) = ((1 - ‖Y‖²), 2Y)/(1 + ‖Y‖²), its derivative with respect to the
   --  length ratio, and its derivatives with respect to the closure coefficients. The chain rule is
   --
   --     ∂T/∂x = Jₛ(Y)·∂Y/∂x,
   --
   --  where Jₛ is the inverse-stereographic Jacobian.

   procedure Sphere_Row
     (Y_Row, Y1, Y2, Y3, D1, D2, D3 : Dimensionless;
      Row                           : Chart_Component_Index;
      Inverse, Quartic, Envelope    : Dimensionless;
      T, D_T, JE_1, JE_2, JE_3      : out Dimensionless)
   is
      pragma Suppress (Overflow_Check);
      Scaled : constant Dimensionless := Quartic * Y_Row;
      J1     : constant Dimensionless := (if Row = 1 then 2.0 * Inverse else 0.0) - Scaled * Y1;
      J2     : constant Dimensionless := (if Row = 2 then 2.0 * Inverse else 0.0) - Scaled * Y2;
      J3     : constant Dimensionless := (if Row = 3 then 2.0 * Inverse else 0.0) - Scaled * Y3;
   begin
      pragma Assert (abs Scaled <= 2.0**230);
      pragma Assert (abs J1 <= 2.0**459 and then abs J2 <= 2.0**459 and then abs J3 <= 2.0**459);
      T := 2.0 * Y_Row * Inverse;
      D_T := 0.0 + J1 * D1 + J2 * D2 + J3 * D3;
      JE_1 := J1 * Envelope;
      JE_2 := J2 * Envelope;
      JE_3 := J3 * Envelope;
   end Sphere_Row;
   pragma Inline (Sphere_Row);

   procedure Evaluate_Node (Y, D_Y : Chart_Vector; V : Dimensionless; T, D_T, JE_1, JE_2, JE_3 : out Raw_Vector_4)
   with
     Pre  =>
       Within (Y, Closure_Point_Limit) and then Within (D_Y, Closure_Point_Limit) and then V >= 0.0 and then V <= 1.0,
     Post =>
       Within (T, Node_Limit)
       and then Within (D_T, Node_Limit)
       and then Within (JE_1, Node_Limit)
       and then Within (JE_2, Node_Limit)
       and then Within (JE_3, Node_Limit);
   --  The tangent at one quadrature node, its derivative with respect to the length ratio, and its derivatives
   --  with respect to the three closure coefficients, the last already multiplied by the closure envelope.

   procedure Evaluate_Node (Y, D_Y : Chart_Vector; V : Dimensionless; T, D_T, JE_1, JE_2, JE_3 : out Raw_Vector_4) is
      pragma Suppress (Overflow_Check);
      Y1                           : constant Dimensionless := Y (1);
      Y2                           : constant Dimensionless := Y (2);
      Y3                           : constant Dimensionless := Y (3);
      D1                           : constant Dimensionless := D_Y (1);
      D2                           : constant Dimensionless := D_Y (2);
      D3                           : constant Dimensionless := D_Y (3);
      pragma Assert (abs Y1 <= Closure_Point_Limit and then abs Y2 <= Closure_Point_Limit);
      pragma Assert (abs Y3 <= Closure_Point_Limit and then abs D1 <= Closure_Point_Limit);
      pragma Assert (abs D2 <= Closure_Point_Limit and then abs D3 <= Closure_Point_Limit);
      R2                           : constant Dimensionless := 0.0 + Y1 * Y1 + Y2 * Y2 + Y3 * Y3;
      pragma Assert (R2 >= 0.0 and then R2 <= 2.0**458);
      Inverse                      : constant Dimensionless := 1.0 / (1.0 + R2);
      pragma Assert (Inverse >= 0.0 and then Inverse <= 1.0);
      Quartic                      : constant Dimensionless := 4.0 * Inverse * Inverse;
      pragma Assert (Quartic >= 0.0 and then Quartic <= 4.0);
      --  256·V⁴·(1 - V)⁴, the endpoint-invisible closure envelope.
      Product                      : constant Dimensionless := V * (1.0 - V);
      Square                       : constant Dimensionless := Product * Product;
      Envelope                     : constant Dimensionless := 256.0 * Square * Square;
      pragma Assert (Product >= 0.0 and then Product <= 1.0);
      pragma Assert (Square >= 0.0 and then Square <= 1.0);
      pragma Assert (Envelope >= 0.0 and then Envelope <= 256.0);
      --  The first component, (1 - ‖Y‖²)/(1 + ‖Y‖²), and its derivatives.
      T0                           : constant Dimensionless := (1.0 - R2) * Inverse;
      J01                          : constant Dimensionless := -Quartic * Y1;
      J02                          : constant Dimensionless := -Quartic * Y2;
      J03                          : constant Dimensionless := -Quartic * Y3;
      pragma Assert (abs T0 <= 2.0**459);
      pragma Assert (abs J01 <= 2.0**230 and then abs J02 <= 2.0**230 and then abs J03 <= 2.0**230);
      D_T0                         : constant Dimensionless := 0.0 + J01 * D1 + J02 * D2 + J03 * D3;
      pragma Assert (abs D_T0 <= 2.0**460);
      T1, T2, T3, D_T1, D_T2, D_T3 : Dimensionless;
      JE_11, JE_12, JE_13          : Dimensionless;
      JE_21, JE_22, JE_23          : Dimensionless;
      JE_31, JE_32, JE_33          : Dimensionless;
   begin
      Sphere_Row (Y1, Y1, Y2, Y3, D1, D2, D3, 1, Inverse, Quartic, Envelope, T1, D_T1, JE_11, JE_12, JE_13);
      Sphere_Row (Y2, Y1, Y2, Y3, D1, D2, D3, 2, Inverse, Quartic, Envelope, T2, D_T2, JE_21, JE_22, JE_23);
      Sphere_Row (Y3, Y1, Y2, Y3, D1, D2, D3, 3, Inverse, Quartic, Envelope, T3, D_T3, JE_31, JE_32, JE_33);
      T := Node_Vector (T0, T1, T2, T3);
      D_T := Node_Vector (D_T0, D_T1, D_T2, D_T3);
      JE_1 := Node_Vector (J01 * Envelope, JE_11, JE_21, JE_31);
      JE_2 := Node_Vector (J02 * Envelope, JE_12, JE_22, JE_32);
      JE_3 := Node_Vector (J03 * Envelope, JE_13, JE_23, JE_33);
   end Evaluate_Node;
   pragma Inline (Evaluate_Node);

   function Within_Nodes (Value : Raw_Vector_4; Nodes : Natural) return Boolean
   is (for all I in Frame_Component_Index => abs Value (I) <= Dimensionless (Nodes) * Node_Limit)
   with Pre => Nodes <= Maximum_Nodes;
   --  Whether every component of a quadrature sum is within what the given number of nodes can have added.

   function Next_Node_Sum (Sum, Term, Weight : Dimensionless; Nodes_Before : Natural) return Dimensionless
   with
     Pre  =>
       Nodes_Before < Maximum_Nodes
       and then abs Sum <= Dimensionless (Nodes_Before) * Node_Limit
       and then abs Term <= Node_Limit
       and then Weight >= 0.0
       and then Weight <= 1.0,
     Post => abs Next_Node_Sum'Result <= Dimensionless (Nodes_Before + 1) * Node_Limit;
   pragma Inline (Next_Node_Sum);
   --  One component of a quadrature sum with one more node's weighted term. The step is stated for scalars alone,
   --  which is what the provers settle quickly.

   function Next_Node_Sum (Sum, Term, Weight : Dimensionless; Nodes_Before : Natural) return Dimensionless is
      pragma Suppress (Overflow_Check);
      pragma Unreferenced (Nodes_Before);
   begin
      return Sum + Term * Weight;
   end Next_Node_Sum;

   function Advanced (Sum, Term : Raw_Vector_4; Weight : Dimensionless; Nodes_Before : Natural) return Raw_Vector_4
   with
     Pre  =>
       Nodes_Before < Maximum_Nodes
       and then Within_Nodes (Sum, Nodes_Before)
       and then Within (Term, Node_Limit)
       and then Weight >= 0.0
       and then Weight <= 1.0,
     Post => Within_Nodes (Advanced'Result, Nodes_Before + 1);
   --  A quadrature sum with one more node's weighted term.

   function Advanced (Sum, Term : Raw_Vector_4; Weight : Dimensionless; Nodes_Before : Natural) return Raw_Vector_4 is
      pragma Suppress (Overflow_Check);
      Result : Raw_Vector_4 := [others => 0.0];
   begin
      for I in Frame_Component_Index loop
         pragma
           Loop_Invariant
             (for all K in Frame_Component_Index => abs Result (K) <= Dimensionless (Nodes_Before + 1) * Node_Limit);
         pragma Assert (abs Sum (I) <= Dimensionless (Nodes_Before) * Node_Limit);
         pragma Assert (abs Term (I) <= Node_Limit);
         Result (I) := Next_Node_Sum (Sum (I), Term (I), Weight, Nodes_Before);
      end loop;
      return Result;
   end Advanced;
   pragma Inline (Advanced);

   procedure Accumulate_Node
     (Chart, D_Chart                                     : Power_Chart;
      U, Weight, Warp_Factor                             : Dimensionless;
      Integral, D_Integral_D_Lambda, D_C_1, D_C_2, D_C_3 : in out Raw_Vector_4;
      Nodes_Before                                       : Natural)
   with
     Pre  =>
       Within (Chart, Closure_Chart_Limit)
       and then Within (D_Chart, Closure_Chart_Limit)
       and then Weight >= 0.0
       and then Weight <= 1.0
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Nodes_Before < Maximum_Nodes
       and then Within_Nodes (Integral, Nodes_Before)
       and then Within_Nodes (D_Integral_D_Lambda, Nodes_Before)
       and then Within_Nodes (D_C_1, Nodes_Before)
       and then Within_Nodes (D_C_2, Nodes_Before)
       and then Within_Nodes (D_C_3, Nodes_Before),
     Post =>
       Within_Nodes (Integral, Nodes_Before + 1)
       and then Within_Nodes (D_Integral_D_Lambda, Nodes_Before + 1)
       and then Within_Nodes (D_C_1, Nodes_Before + 1)
       and then Within_Nodes (D_C_2, Nodes_Before + 1)
       and then Within_Nodes (D_C_3, Nodes_Before + 1);
   --  Add one quadrature node to T(U), ∂T/∂λ, and the three ∂T/∂C₀ components. This is the innermost loop of the
   --  closure solve.

   procedure Accumulate_Node
     (Chart, D_Chart                                     : Power_Chart;
      U, Weight, Warp_Factor                             : Dimensionless;
      Integral, D_Integral_D_Lambda, D_C_1, D_C_2, D_C_3 : in out Raw_Vector_4;
      Nodes_Before                                       : Natural)
   is
      pragma Suppress (Overflow_Check);
      --  Every node lies strictly inside the unit interval. Taking it between zero and one says so without an
      --  argument about the rounding of the panel arithmetic. Written as tests, not as 'Min and 'Max, which GNAT
      --  guards with a test for a NaN.
      V                        : constant Dimensionless :=
        Warp_Parameter ((if U >= 1.0 then 1.0 elsif U >= 0.0 then U else 0.0), Warp_Factor);
      Y                        : constant Chart_Vector :=
        Closure_Point (Horner_Value (Chart, 1, V), Horner_Value (Chart, 2, V), Horner_Value (Chart, 3, V));
      D_Y                      : constant Chart_Vector :=
        Closure_Point (Horner_Value (D_Chart, 1, V), Horner_Value (D_Chart, 2, V), Horner_Value (D_Chart, 3, V));
      T, D_T, JE_1, JE_2, JE_3 : Raw_Vector_4;
   begin
      Evaluate_Node (Y, D_Y, V, T, D_T, JE_1, JE_2, JE_3);
      Integral := Advanced (Integral, T, Weight, Nodes_Before);
      D_Integral_D_Lambda := Advanced (D_Integral_D_Lambda, D_T, Weight, Nodes_Before);
      D_C_1 := Advanced (D_C_1, JE_1, Weight, Nodes_Before);
      D_C_2 := Advanced (D_C_2, JE_2, Weight, Nodes_Before);
      D_C_3 := Advanced (D_C_3, JE_3, Weight, Nodes_Before);
   end Accumulate_Node;

   Closure_Vector_Limit : constant := 8.0 * Tame_Limit;
   --  Above a component of a frame combination of quadrature sums within Tame_Limit.

   function Closure_Global_Residual
     (Integral : Position_Scale; Lambda : Dimensionless; Chord_Direction : Position_Scale) return Position_Scale
   with
     Pre  =>
       Within (Integral, Closure_Vector_Limit)
       and then abs Lambda <= Lambda_Limit
       and then Within (Chord_Direction, Candidate_Component_Limit),
     Post => Within (Closure_Global_Residual'Result, Frame_Operand_Limit);
   --  λ·∫₀¹T(U)dU - c on the machine axes.

   function Closure_Global_Residual
     (Integral : Position_Scale; Lambda : Dimensionless; Chord_Direction : Position_Scale) return Position_Scale
   is
      Result : Position_Scale := [others => 0.0];
   begin
      for A in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => abs Result (B) <= Frame_Operand_Limit);
         pragma Assert (abs Integral (A) <= Closure_Vector_Limit);
         pragma Assert (abs Chord_Direction (A) <= Candidate_Component_Limit);
         declare
            Component : constant Dimensionless := Integral (A) * Lambda - Chord_Direction (A);
         begin
            pragma Assert (abs Component <= Frame_Operand_Limit);
            Result (A) := Component;
         end;
      end loop;
      return Result;
   end Closure_Global_Residual;

   function Closure_Global_Slope (Integral, D_Integral : Position_Scale; Lambda : Dimensionless) return Position_Scale
   with
     Pre  =>
       Within (Integral, Closure_Vector_Limit)
       and then Within (D_Integral, Closure_Vector_Limit)
       and then abs Lambda <= Lambda_Limit,
     Post => Within (Closure_Global_Slope'Result, Frame_Operand_Limit);
   --  The derivative of that residual with respect to λ: the integral plus λ times its own derivative.

   function Closure_Global_Slope (Integral, D_Integral : Position_Scale; Lambda : Dimensionless) return Position_Scale
   is
      Result : Position_Scale := [others => 0.0];
   begin
      for A in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => abs Result (B) <= Frame_Operand_Limit);
         pragma Assert (abs Integral (A) <= Closure_Vector_Limit and then abs D_Integral (A) <= Closure_Vector_Limit);
         declare
            Component : constant Dimensionless := Integral (A) + D_Integral (A) * Lambda;
         begin
            pragma Assert (abs Component <= Frame_Operand_Limit);
            Result (A) := Component;
         end;
      end loop;
      return Result;
   end Closure_Global_Slope;

   function Scaled_By_Lambda (Coordinate, Lambda : Dimensionless) return Dimensionless
   is (Lambda * Coordinate)
   with Pre => abs Coordinate <= Frame_Product_Limit and then abs Lambda <= Lambda_Limit;
   pragma Inline (Scaled_By_Lambda);

   procedure Closure_Residual_From_Terms
     (Terms                : Chart_Lambda_Terms;
      Frame, Closure_Frame : Frame_Vector_Array;
      Chord_Direction      : Position_Scale;
      Lambda               : Dimensionless;
      C0                   : Raw_Vector_3;
      Warp_Factor          : Dimensionless;
      Residual             : out Raw_Vector_4;
      Jacobian_C           : out Raw_Matrix_4_3;
      D_Residual_D_Lambda  : out Raw_Vector_4;
      Usable               : out Boolean;
      Panel_Count          : Positive)
   with
     Pre =>
       Within (Terms, Tame_Limit)
       and then Within (Frame, Unit_Component_Limit)
       and then Within (Closure_Frame, Unit_Component_Limit)
       and then Within (Chord_Direction, Candidate_Component_Limit)
       and then abs Lambda <= Lambda_Limit
       and then Within (C0, Tame_Limit)
       and then Warp_Factor >= Minimum_Distance_Warp_Factor
       and then Warp_Factor <= Maximum_Distance_Warp_Factor
       and then Panel_Count <= Maximum_Panel_Count;
   --  Evaluate the four-component position-closure residual and its derivatives with respect to the three closure
   --  coefficients and Lambda. Panel_Count controls composite quadrature resolution for the nonlinear solve. Usable
   --  is False, with the results zero, if a quadrature sum is beyond Tame_Limit, as a NaN is.

   procedure Closure_Residual_From_Terms
     (Terms                : Chart_Lambda_Terms;
      Frame, Closure_Frame : Frame_Vector_Array;
      Chord_Direction      : Position_Scale;
      Lambda               : Dimensionless;
      C0                   : Raw_Vector_3;
      Warp_Factor          : Dimensionless;
      Residual             : out Raw_Vector_4;
      Jacobian_C           : out Raw_Matrix_4_3;
      D_Residual_D_Lambda  : out Raw_Vector_4;
      Usable               : out Boolean;
      Panel_Count          : Positive)
   is
      Power_Coefficients, D_Power_Coefficients           : Power_Chart;
      Integral, D_Integral_D_Lambda, D_C_1, D_C_2, D_C_3 : Raw_Vector_4 := [others => 0.0];
      Count                                              : Natural := 0;
   begin
      Residual := [others => 0.0];
      Jacobian_C := [others => [others => 0.0]];
      D_Residual_D_Lambda := [others => 0.0];
      Usable := False;

      Chart_At_Lambda (Terms, Lambda, C0, Power_Coefficients, D_Power_Coefficients);
      --  The direct Newton proposal uses a fixed composite rule. A separately certified integral gates every
      --  accepted candidate.
      for Segment in 0 .. Panel_Count - 1 loop
         pragma Loop_Invariant (Count = 16 * Segment);
         pragma
           Loop_Invariant
             (Within_Nodes (Integral, Count)
              and then Within_Nodes (D_Integral_D_Lambda, Count)
              and then Within_Nodes (D_C_1, Count)
              and then Within_Nodes (D_C_2, Count)
              and then Within_Nodes (D_C_3, Count));
         declare
            Middle : constant Dimensionless := (Dimensionless (Segment) + 0.5) / Dimensionless (Panel_Count);
            Half   : constant Dimensionless := 0.5 / Dimensionless (Panel_Count);
         begin
            pragma Assert (Half >= 0.0 and then Half <= 0.5);
            pragma Assert (abs Middle <= 128.0);
            for I in GL16_Positive_Index loop
               pragma Loop_Invariant (Count = 16 * Segment + 2 * (I - 1));
               pragma
                 Loop_Invariant
                   (Within_Nodes (Integral, Count)
                    and then Within_Nodes (D_Integral_D_Lambda, Count)
                    and then Within_Nodes (D_C_1, Count)
                    and then Within_Nodes (D_C_2, Count)
                    and then Within_Nodes (D_C_3, Count));
               pragma Assert (GL16_Weights (I) >= 0.0 and then GL16_Weights (I) <= 1.0);
               pragma Assert (GL16_Nodes (I) >= 0.0 and then GL16_Nodes (I) <= 1.0);
               declare
                  Weight : constant Dimensionless := Half * GL16_Weights (I);
                  Offset : constant Dimensionless := Half * GL16_Nodes (I);
               begin
                  pragma Assert (Weight >= 0.0 and then Weight <= 1.0);
                  Accumulate_Node
                    (Power_Coefficients,
                     D_Power_Coefficients,
                     Middle - Offset,
                     Weight,
                     Warp_Factor,
                     Integral,
                     D_Integral_D_Lambda,
                     D_C_1,
                     D_C_2,
                     D_C_3,
                     Count);
                  Count := Count + 1;
                  Accumulate_Node
                    (Power_Coefficients,
                     D_Power_Coefficients,
                     Middle + Offset,
                     Weight,
                     Warp_Factor,
                     Integral,
                     D_Integral_D_Lambda,
                     D_C_1,
                     D_C_2,
                     D_C_3,
                     Count);
                  Count := Count + 1;
               end;
            end loop;
         end;
      end loop;

      --  The tangent is a unit vector, so its integral is at most one, and its derivatives are of moderate size
      --  for any chart worth solving. Sums beyond the common limit are reported instead of being resolved in the
      --  frames.
      if not Within (Integral, Tame_Limit)
        or else not Within (D_Integral_D_Lambda, Tame_Limit)
        or else not Within (D_C_1, Tame_Limit)
        or else not Within (D_C_2, Tame_Limit)
        or else not Within (D_C_3, Tame_Limit)
      then
         return;
      end if;

      declare
         Global_Integral   : constant Position_Scale := Frame_Vector (Frame, Integral);
         Global_D_Integral : constant Position_Scale := Frame_Vector (Frame, D_Integral_D_Lambda);
         Global_D_C_1      : constant Position_Scale := Frame_Vector (Frame, D_C_1);
         Global_D_C_2      : constant Position_Scale := Frame_Vector (Frame, D_C_2);
         Global_D_C_3      : constant Position_Scale := Frame_Vector (Frame, D_C_3);
         Global_Residual   : Position_Scale;
         Global_D_Lambda   : Position_Scale;
      begin
         pragma Assert (Within (Global_Integral, Closure_Vector_Limit));
         pragma Assert (Within (Global_D_Integral, Closure_Vector_Limit));
         Lemma_Within_Larger (Global_D_C_1, Closure_Vector_Limit, Frame_Operand_Limit);
         Lemma_Within_Larger (Global_D_C_2, Closure_Vector_Limit, Frame_Operand_Limit);
         Lemma_Within_Larger (Global_D_C_3, Closure_Vector_Limit, Frame_Operand_Limit);
         Global_Residual := Closure_Global_Residual (Global_Integral, Lambda, Chord_Direction);
         Global_D_Lambda := Closure_Global_Slope (Global_Integral, Global_D_Integral, Lambda);
         --  Closure in chord-normalized coordinates is
         --
         --     R(λ, C₀) = λ·∫₀¹T(U; λ, C₀)dU - c = 0.
         --
         --  Resolve the four physical residual components into Closure_Frame so the Newton system is well scaled.
         for I in Frame_Component_Index loop
            pragma Assert (Within (Closure_Frame (I), Unit_Component_Limit));
            Residual (I) := Frame_Coordinate (Closure_Frame, I, Global_Residual);
            D_Residual_D_Lambda (I) := Frame_Coordinate (Closure_Frame, I, Global_D_Lambda);
            declare
               Coordinate_1 : constant Dimensionless := Frame_Coordinate (Closure_Frame, I, Global_D_C_1);
               Coordinate_2 : constant Dimensionless := Frame_Coordinate (Closure_Frame, I, Global_D_C_2);
               Coordinate_3 : constant Dimensionless := Frame_Coordinate (Closure_Frame, I, Global_D_C_3);
            begin
               Jacobian_C (I, 1) := Scaled_By_Lambda (Coordinate_1, Lambda);
               Jacobian_C (I, 2) := Scaled_By_Lambda (Coordinate_2, Lambda);
               Jacobian_C (I, 3) := Scaled_By_Lambda (Coordinate_3, Lambda);
            end;
         end loop;
      end;
      Usable := True;
   end Closure_Residual_From_Terms;

   --  The Newton step is the solution of a four-by-four linear system, found by Gaussian elimination with partial
   --  pivoting on a matrix augmented with the right-hand side.

   type Augmented_Matrix is array (Frame_Component_Index, Natural range 0 .. 4) of Dimensionless;

   function Within (Value : Augmented_Matrix; Limit : Dimensionless) return Boolean
   is (for all Row in Frame_Component_Index => (for all Column in 0 .. 4 => abs Value (Row, Column) <= Limit));

   function Within (Value : Raw_Matrix_4; Limit : Dimensionless) return Boolean
   is (for all Row in Frame_Component_Index =>
         (for all Column in Frame_Component_Index => abs Value (Row, Column) <= Limit));

   function Within (Value : Raw_Matrix_4_3; Limit : Dimensionless) return Boolean
   is (for all Row in Frame_Component_Index =>
         (for all Column in Chart_Component_Index => abs Value (Row, Column) <= Limit));

   Smallest_Pivot : constant Dimensionless := 1.0E-20;
   --  Reject a pivot that is too small to support a trustworthy Newton direction.

   Linear_Solution_Limit : constant := 2.0**700;
   --  Above every component of a solution: entries within 2¹⁰⁴ after elimination, divided by a pivot above
   --  10⁻²⁰ once for each of the four unknowns in turn.

   function Elimination_Limit (Column : Natural) return Dimensionless
   is (case Column is
         when 0      => 2.0**100,
         when 1      => 2.0**101,
         when 2      => 2.0**102,
         when 3      => 2.0**103,
         when others => 2.0**104)
   with Ghost;
   --  Above every entry before the given column is eliminated. A factor is at most one in magnitude, so each
   --  elimination at most doubles an entry.

   procedure Set_Entry
     (Work : in out Augmented_Matrix; Row : Frame_Component_Index; Column : Natural; Value, Limit : Dimensionless)
   with Pre => Column <= 4 and then Within (Work, Limit) and then abs Value <= Limit, Post => Within (Work, Limit);

   procedure Set_Entry
     (Work : in out Augmented_Matrix; Row : Frame_Component_Index; Column : Natural; Value, Limit : Dimensionless)
   is
      pragma Unreferenced (Limit);
   begin
      Work (Row, Column) := Value;
   end Set_Entry;
   pragma Inline (Set_Entry);

   procedure Lemma_Work_Within_Larger (Work : Augmented_Matrix; Smaller, Larger : Dimensionless)
   with Ghost, Pre => Within (Work, Smaller) and then Smaller <= Larger, Post => Within (Work, Larger);

   procedure Lemma_Work_Within_Larger (Work : Augmented_Matrix; Smaller, Larger : Dimensionless) is null;

   function Eliminated_Entry (Own, Factor, Pivot_Row, Limit : Dimensionless) return Dimensionless
   is (Own - Factor * Pivot_Row)
   with
     Pre  =>
       (Limit = 2.0**100 or else Limit = 2.0**101 or else Limit = 2.0**102 or else Limit = 2.0**103)
       and then abs Own <= Limit
       and then abs Pivot_Row <= Limit
       and then abs Factor <= 1.0,
     Post => abs Eliminated_Entry'Result <= Limit + Limit;
   pragma Inline (Eliminated_Entry);

   procedure Eliminate_Column (Work : in out Augmented_Matrix; Column : Frame_Component_Index; Usable : out Boolean)
   with Pre => Within (Work, Elimination_Limit (Column)), Post => Within (Work, Elimination_Limit (Column + 1));
   --  Bring the largest remaining entry of the column to the diagonal and clear the column below it. Usable is
   --  False if that entry is too small to divide by.

   procedure Eliminate_Column (Work : in out Augmented_Matrix; Column : Frame_Component_Index; Usable : out Boolean) is
      Limit : constant Dimensionless :=
        (case Column is
           when 0 => 2.0**100,
           when 1 => 2.0**101,
           when 2 => 2.0**102,
           when 3 => 2.0**103);
      Pivot : Frame_Component_Index := Column;
   begin
      pragma Assert (Limit = Elimination_Limit (Column));
      pragma Assert (Limit + Limit = Elimination_Limit (Column + 1));
      Usable := False;
      for Row in Column .. Frame_Component_Index'Last loop
         pragma Loop_Invariant (Pivot in Column .. Frame_Component_Index'Last);
         if abs Work (Row, Column) > abs Work (Pivot, Column) then
            Pivot := Row;
         end if;
      end loop;
      if not (abs Work (Pivot, Column) > Smallest_Pivot) then
         Lemma_Work_Within_Larger (Work, Limit, Limit + Limit);
         return;
      end if;
      if Pivot /= Column then
         for J in Column .. 4 loop
            pragma Loop_Invariant (Within (Work, Limit));
            pragma Assert (abs Work (Column, J) <= Limit and then abs Work (Pivot, J) <= Limit);
            declare
               Swap : constant Dimensionless := Work (Column, J);
            begin
               Set_Entry (Work, Column, J, Work (Pivot, J), Limit);
               Set_Entry (Work, Pivot, J, Swap, Limit);
            end;
         end loop;
      end if;
      --  The exchange has put the pivot on the diagonal. It is tested again rather than followed through the
      --  exchange, as is each factor below, which partial pivoting makes at most one in magnitude.
      if not (abs Work (Column, Column) > Smallest_Pivot) then
         Lemma_Work_Within_Larger (Work, Limit, Limit + Limit);
         return;
      end if;
      declare
         Before : constant Augmented_Matrix := Work;
      begin
         Lemma_Work_Within_Larger (Work, Limit, Limit + Limit);
         for Row in Column + 1 .. Frame_Component_Index'Last loop
            pragma Loop_Invariant (Within (Work, Limit + Limit));
            pragma Assert (abs Before (Row, Column) <= Limit and then abs Before (Column, Column) <= Limit);
            declare
               Factor : constant Dimensionless := Before (Row, Column) / Before (Column, Column);
            begin
               if not (abs Factor <= 1.0) then
                  return;
               end if;
               for J in Column .. 4 loop
                  pragma Loop_Invariant (Within (Work, Limit + Limit));
                  pragma Assert (abs Before (Row, J) <= Limit and then abs Before (Column, J) <= Limit);
                  Set_Entry
                    (Work,
                     Row,
                     J,
                     Eliminated_Entry (Before (Row, J), Factor, Before (Column, J), Limit),
                     Limit + Limit);
               end loop;
            end;
         end loop;
      end;
      Usable := True;
   end Eliminate_Column;

   function Linear_Solution (S0, S1, S2, S3 : Dimensionless) return Raw_Vector_4
   is ([0 => S0, 1 => S1, 2 => S2, 3 => S3])
   with
     Pre  =>
       abs S0 <= Linear_Solution_Limit
       and then abs S1 <= Linear_Solution_Limit
       and then abs S2 <= Linear_Solution_Limit
       and then abs S3 <= Linear_Solution_Limit,
     Post => Within (Linear_Solution'Result, Linear_Solution_Limit);

   function Back_Substituted (Right, Pivot : Dimensionless) return Dimensionless
   is (Right / Pivot)
   with
     Pre  => abs Pivot > Smallest_Pivot and then abs Right <= 2.0**630,
     Post =>
       abs Back_Substituted'Result <= Linear_Solution_Limit
       and then (if abs Right <= 2.0**104 then abs Back_Substituted'Result <= 2.0**171)
       and then (if abs Right <= 2.0**276 then abs Back_Substituted'Result <= 2.0**343)
       and then (if abs Right <= 2.0**448 then abs Back_Substituted'Result <= 2.0**515);
   pragma Inline (Back_Substituted);

   function Reduced_Third (Right, W23, S3 : Dimensionless) return Dimensionless
   is (Right - W23 * S3)
   with
     Pre  => abs Right <= 2.0**104 and then abs W23 <= 2.0**104 and then abs S3 <= 2.0**171,
     Post => abs Reduced_Third'Result <= 2.0**276;

   function Reduced_Second (Right, W12, S2, W13, S3 : Dimensionless) return Dimensionless
   is (Right - W12 * S2 - W13 * S3)
   with
     Pre  =>
       abs Right <= 2.0**104
       and then abs W12 <= 2.0**104
       and then abs W13 <= 2.0**104
       and then abs S2 <= 2.0**343
       and then abs S3 <= 2.0**171,
     Post => abs Reduced_Second'Result <= 2.0**448;

   function Reduced_First (Right, W01, S1, W02, S2, W03, S3 : Dimensionless) return Dimensionless
   is (Right - W01 * S1 - W02 * S2 - W03 * S3)
   with
     Pre  =>
       abs Right <= 2.0**104
       and then abs W01 <= 2.0**104
       and then abs W02 <= 2.0**104
       and then abs W03 <= 2.0**104
       and then abs S1 <= 2.0**515
       and then abs S2 <= 2.0**343
       and then abs S3 <= 2.0**171,
     Post => abs Reduced_First'Result <= 2.0**620;
   --  The right-hand side of a row less what the unknowns already found contribute, in the order of the columns.

   procedure Back_Substitute (Work : Augmented_Matrix; Solution : out Raw_Vector_4; Usable : out Boolean)
   with Pre => Within (Work, 2.0**104), Post => Within (Solution, Linear_Solution_Limit);
   --  Solve the triangular system which elimination left. Usable is False, with a zero solution, if a diagonal
   --  entry is too small to divide by.

   procedure Back_Substitute (Work : Augmented_Matrix; Solution : out Raw_Vector_4; Usable : out Boolean) is
      W01            : constant Dimensionless := Work (0, 1);
      W02            : constant Dimensionless := Work (0, 2);
      W03            : constant Dimensionless := Work (0, 3);
      W12            : constant Dimensionless := Work (1, 2);
      W13            : constant Dimensionless := Work (1, 3);
      W23            : constant Dimensionless := Work (2, 3);
      R0             : constant Dimensionless := Work (0, 4);
      R1             : constant Dimensionless := Work (1, 4);
      R2             : constant Dimensionless := Work (2, 4);
      R3             : constant Dimensionless := Work (3, 4);
      pragma Assert (abs W01 <= 2.0**104 and then abs W02 <= 2.0**104 and then abs W03 <= 2.0**104);
      pragma Assert (abs W12 <= 2.0**104 and then abs W13 <= 2.0**104 and then abs W23 <= 2.0**104);
      pragma Assert (abs R0 <= 2.0**104 and then abs R1 <= 2.0**104);
      pragma Assert (abs R2 <= 2.0**104 and then abs R3 <= 2.0**104);
      S0, S1, S2, S3 : Dimensionless;
      N0, N1, N2     : Dimensionless;
   begin
      Solution := [others => 0.0];
      Usable := False;
      if not (abs Work (0, 0) > Smallest_Pivot)
        or else not (abs Work (1, 1) > Smallest_Pivot)
        or else not (abs Work (2, 2) > Smallest_Pivot)
        or else not (abs Work (3, 3) > Smallest_Pivot)
      then
         return;
      end if;
      S3 := Back_Substituted (R3, Work (3, 3));
      pragma Assert (abs S3 <= 2.0**171);
      N2 := Reduced_Third (R2, W23, S3);
      S2 := Back_Substituted (N2, Work (2, 2));
      pragma Assert (abs S2 <= 2.0**343);
      N1 := Reduced_Second (R1, W12, S2, W13, S3);
      S1 := Back_Substituted (N1, Work (1, 1));
      pragma Assert (abs S1 <= 2.0**515);
      N0 := Reduced_First (R0, W01, S1, W02, S2, W03, S3);
      S0 := Back_Substituted (N0, Work (0, 0));
      Solution := Linear_Solution (S0, S1, S2, S3);
      Usable := True;
   end Back_Substitute;

   function Augmented (Matrix : Raw_Matrix_4; Right : Raw_Vector_4) return Augmented_Matrix
   is ([for Row in Frame_Component_Index =>
          [for Column in 0 .. 4 => (if Column = 4 then Right (Row) else Matrix (Row, Column))]])
   with
     Pre  => Within (Matrix, Tame_Limit) and then Within (Right, Tame_Limit),
     Post => Within (Augmented'Result, Tame_Limit);

   function Solve_Linear (Matrix : Raw_Matrix_4; Right : Raw_Vector_4; Solution : out Raw_Vector_4) return Boolean
   with
     Side_Effects,
     Pre  => Within (Matrix, Tame_Limit) and then Within (Right, Tame_Limit),
     Post => Within (Solution, Linear_Solution_Limit);
   --  Solve a four-by-four linear system and report whether a stable solution was found.

   function Solve_Linear (Matrix : Raw_Matrix_4; Right : Raw_Vector_4; Solution : out Raw_Vector_4) return Boolean is
      Work   : Augmented_Matrix := Augmented (Matrix, Right);
      Usable : Boolean;
   begin
      Solution := [others => 0.0];
      pragma Assert (Within (Solution, Linear_Solution_Limit));
      --  Gaussian elimination with partial pivoting solves J·Δ = -R.
      for Column in Frame_Component_Index loop
         pragma Loop_Invariant (Within (Work, Elimination_Limit (Column)));
         pragma Loop_Invariant (Within (Solution, Linear_Solution_Limit));
         Eliminate_Column (Work, Column, Usable);
         if not Usable then
            return False;
         end if;
      end loop;
      Back_Substitute (Work, Solution, Usable);
      return Usable;
   end Solve_Linear;

   function Is_Tame_Evaluation (Residual, D_Lambda : Raw_Vector_4; Jacobian_C : Raw_Matrix_4_3) return Boolean
   is (Within (Residual, Tame_Limit) and then Within (D_Lambda, Tame_Limit) and then Within (Jacobian_C, Tame_Limit));
   --  Whether a closure residual and its derivatives are within the limit the linear algebra of the solve takes.
   --  They are of order one for any chart worth solving.

   function Residual_Norm (Value : Raw_Vector_4) return Dimensionless
   with
     Pre  => Within (Value, Tame_Limit),
     Post => Residual_Norm'Result >= 0.0 and then Residual_Norm'Result <= 2.0**101;

   function Residual_Norm (Value : Raw_Vector_4) return Dimensionless is
      V0  : constant Dimensionless := Value (0);
      V1  : constant Dimensionless := Value (1);
      V2  : constant Dimensionless := Value (2);
      V3  : constant Dimensionless := Value (3);
      pragma Assert (abs V0 <= Tame_Limit and then abs V1 <= Tame_Limit);
      pragma Assert (abs V2 <= Tame_Limit and then abs V3 <= Tame_Limit);
      Sum : constant Dimensionless := V0 * V0 + V1 * V1 + V2 * V2 + V3 * V3;
   begin
      pragma Assert (Sum >= 0.0 and then Sum <= 2.0**202);
      Stereographic_Kernel.Elementary.Lemma_Sqrt_Is_At_Most (Sum, 2.0**101);
      return Dimensionless_Math.Sqrt (Sum);
   end Residual_Norm;

   function Squared_Norm (Value : Raw_Vector_4) return Dimensionless
   with Pre => Within (Value, Tame_Limit), Post => Squared_Norm'Result >= 0.0 and then Squared_Norm'Result <= 2.0**202;

   function Squared_Norm (Value : Raw_Vector_4) return Dimensionless is
      V0 : constant Dimensionless := Value (0);
      V1 : constant Dimensionless := Value (1);
      V2 : constant Dimensionless := Value (2);
      V3 : constant Dimensionless := Value (3);
      pragma Assert (abs V0 <= Tame_Limit and then abs V1 <= Tame_Limit);
      pragma Assert (abs V2 <= Tame_Limit and then abs V3 <= Tame_Limit);
   begin
      return 0.0 + V0 * V0 + V1 * V1 + V2 * V2 + V3 * V3;
   end Squared_Norm;

   function Newton_Matrix (D_Lambda : Raw_Vector_4; Jacobian_C : Raw_Matrix_4_3) return Raw_Matrix_4
   is ([for Row in Frame_Component_Index =>
          [for Column in Frame_Component_Index => (if Column = 0 then D_Lambda (Row) else Jacobian_C (Row, Column))]])
   with
     Pre  => Within (D_Lambda, Tame_Limit) and then Within (Jacobian_C, Tame_Limit),
     Post => Within (Newton_Matrix'Result, Tame_Limit);
   --  The Jacobian of the residual with respect to λ and the three closure coefficients.

   function Negated (Value : Raw_Vector_4) return Raw_Vector_4
   is ([for I in Frame_Component_Index => -Value (I)])
   with Pre => Within (Value, Tame_Limit), Post => Within (Negated'Result, Tame_Limit);

   function Row_Slope (Residual, J1, J2, J3, S1, S2, S3 : Dimensionless) return Dimensionless
   is (Residual * (0.0 + J1 * S1 + J2 * S2 + J3 * S3))
   with
     Pre  =>
       abs Residual <= Tame_Limit
       and then abs J1 <= Tame_Limit
       and then abs J2 <= Tame_Limit
       and then abs J3 <= Tame_Limit
       and then abs S1 <= Linear_Solution_Limit
       and then abs S2 <= Linear_Solution_Limit
       and then abs S3 <= Linear_Solution_Limit,
     Post => abs Row_Slope'Result <= 2.0**902;

   function C0_Slope (Residual : Raw_Vector_4; Jacobian_C : Raw_Matrix_4_3; Step : Raw_Vector_4) return Dimensionless
   with
     Pre =>
       Within (Residual, Tame_Limit)
       and then Within (Jacobian_C, Tame_Limit)
       and then Within (Step, Linear_Solution_Limit);
   --  R·(J꜀·ΔC₀): half the rate at which ‖R‖² changes along Step while λ stays on its bound.

   function C0_Slope (Residual : Raw_Vector_4; Jacobian_C : Raw_Matrix_4_3; Step : Raw_Vector_4) return Dimensionless
   is
      S1      : constant Dimensionless := Step (1);
      S2      : constant Dimensionless := Step (2);
      S3      : constant Dimensionless := Step (3);
      pragma
        Assert
          (abs S1 <= Linear_Solution_Limit
           and then abs S2 <= Linear_Solution_Limit
           and then abs S3 <= Linear_Solution_Limit);
      pragma
        Assert
          (for all Row in Frame_Component_Index =>
             abs Residual (Row) <= Tame_Limit
             and then abs Jacobian_C (Row, 1) <= Tame_Limit
             and then abs Jacobian_C (Row, 2) <= Tame_Limit
             and then abs Jacobian_C (Row, 3) <= Tame_Limit);
      Slope_0 : constant Dimensionless :=
        Row_Slope (Residual (0), Jacobian_C (0, 1), Jacobian_C (0, 2), Jacobian_C (0, 3), S1, S2, S3);
      Slope_1 : constant Dimensionless :=
        Row_Slope (Residual (1), Jacobian_C (1, 1), Jacobian_C (1, 2), Jacobian_C (1, 3), S1, S2, S3);
      Slope_2 : constant Dimensionless :=
        Row_Slope (Residual (2), Jacobian_C (2, 1), Jacobian_C (2, 2), Jacobian_C (2, 3), S1, S2, S3);
      Slope_3 : constant Dimensionless :=
        Row_Slope (Residual (3), Jacobian_C (3, 1), Jacobian_C (3, 2), Jacobian_C (3, 3), S1, S2, S3);
   begin
      return 0.0 + Slope_0 + Slope_1 + Slope_2 + Slope_3;
   end C0_Slope;

   function Column_Product (A0, A1, A2, A3, B0, B1, B2, B3 : Dimensionless) return Dimensionless
   is (0.0 + A0 * B0 + A1 * B1 + A2 * B2 + A3 * B3)
   with
     Pre  =>
       abs A0 <= Tame_Limit
       and then abs A1 <= Tame_Limit
       and then abs A2 <= Tame_Limit
       and then abs A3 <= Tame_Limit
       and then abs B0 <= Tame_Limit
       and then abs B1 <= Tame_Limit
       and then abs B2 <= Tame_Limit
       and then abs B3 <= Tame_Limit,
     Post => abs Column_Product'Result <= 2.0**202;
   --  The sum of the products of two columns of four entries.

   function Normal_Entry (Jacobian_C : Raw_Matrix_4_3; I, J : Chart_Component_Index) return Dimensionless
   with Pre => Within (Jacobian_C, Tame_Limit), Post => abs Normal_Entry'Result <= 2.0**202;
   --  An entry of J꜀ᵀ·J꜀.

   function Normal_Entry (Jacobian_C : Raw_Matrix_4_3; I, J : Chart_Component_Index) return Dimensionless is
   begin
      pragma
        Assert
          (for all Row in Frame_Component_Index =>
             abs Jacobian_C (Row, I) <= Tame_Limit and then abs Jacobian_C (Row, J) <= Tame_Limit);
      return
        Column_Product
          (Jacobian_C (0, I),
           Jacobian_C (1, I),
           Jacobian_C (2, I),
           Jacobian_C (3, I),
           Jacobian_C (0, J),
           Jacobian_C (1, J),
           Jacobian_C (2, J),
           Jacobian_C (3, J));
   end Normal_Entry;

   function Negated_Column_Product (A0, A1, A2, A3, B0, B1, B2, B3 : Dimensionless) return Dimensionless
   is (0.0 - A0 * B0 - A1 * B1 - A2 * B2 - A3 * B3)
   with
     Pre  =>
       abs A0 <= Tame_Limit
       and then abs A1 <= Tame_Limit
       and then abs A2 <= Tame_Limit
       and then abs A3 <= Tame_Limit
       and then abs B0 <= Tame_Limit
       and then abs B1 <= Tame_Limit
       and then abs B2 <= Tame_Limit
       and then abs B3 <= Tame_Limit,
     Post => abs Negated_Column_Product'Result <= 2.0**202;

   function Gradient_Entry
     (Jacobian_C : Raw_Matrix_4_3; Residual : Raw_Vector_4; I : Chart_Component_Index) return Dimensionless
   with
     Pre  => Within (Jacobian_C, Tame_Limit) and then Within (Residual, Tame_Limit),
     Post => abs Gradient_Entry'Result <= 2.0**202;
   --  An entry of -J꜀ᵀ·R.

   function Gradient_Entry
     (Jacobian_C : Raw_Matrix_4_3; Residual : Raw_Vector_4; I : Chart_Component_Index) return Dimensionless is
   begin
      pragma
        Assert
          (for all Row in Frame_Component_Index =>
             abs Jacobian_C (Row, I) <= Tame_Limit and then abs Residual (Row) <= Tame_Limit);
      return
        Negated_Column_Product
          (Jacobian_C (0, I),
           Jacobian_C (1, I),
           Jacobian_C (2, I),
           Jacobian_C (3, I),
           Residual (0),
           Residual (1),
           Residual (2),
           Residual (3));
   end Gradient_Entry;

   procedure Held_Step_System
     (Jacobian_C : Raw_Matrix_4_3;
      Residual   : Raw_Vector_4;
      Normal     : out Raw_Matrix_4;
      Gradient   : out Raw_Vector_4;
      Usable     : out Boolean)
   with
     Pre  => Within (Jacobian_C, Tame_Limit) and then Within (Residual, Tame_Limit),
     Post => (if Usable then Within (Normal, Tame_Limit) and then Within (Gradient, Tame_Limit));
   --  The normal equations J꜀ᵀ·J꜀·ΔC₀ = -J꜀ᵀ·R for the Gauss-Newton step with λ held; the λ row is the identity,
   --  so its component is zero. Usable is False if an entry is beyond the limit of the linear solve.

   procedure Held_Step_System
     (Jacobian_C : Raw_Matrix_4_3;
      Residual   : Raw_Vector_4;
      Normal     : out Raw_Matrix_4;
      Gradient   : out Raw_Vector_4;
      Usable     : out Boolean) is
   begin
      Normal :=
        [for I in Frame_Component_Index =>
           [for J in Frame_Component_Index =>
              (if I = 0 or else J = 0 then (if I = J then 1.0 else 0.0) else Normal_Entry (Jacobian_C, I, J))]];
      Gradient :=
        [for I in Frame_Component_Index => (if I = 0 then 0.0 else Gradient_Entry (Jacobian_C, Residual, I))];
      Usable := Within (Normal, Tame_Limit) and then Within (Gradient, Tame_Limit);
   end Held_Step_System;

   function Trial_Coefficients (C0 : Raw_Vector_3; Step : Raw_Vector_4; Scale : Dimensionless) return Raw_Vector_3
   with
     Pre =>
       Within (C0, Tame_Limit)
       and then Within (Step, Linear_Solution_Limit)
       and then Scale >= 0.0
       and then Scale <= 1.0;
   --  The closure coefficients moved by a fraction of the step.

   function Trial_Coefficients (C0 : Raw_Vector_3; Step : Raw_Vector_4; Scale : Dimensionless) return Raw_Vector_3 is
   begin
      pragma
        Assert
          (for all Column in Chart_Component_Index =>
             abs C0 (Column) <= Tame_Limit and then abs Step (Column) <= Linear_Solution_Limit);
      return [1 => C0 (1) + Scale * Step (1), 2 => C0 (2) + Scale * Step (2), 3 => C0 (3) + Scale * Step (3)];
   end Trial_Coefficients;

   function Solve_Closure_From_Terms
     (Terms                     : Chart_Lambda_Terms;
      Frame                     : Frame_Vector_Array;
      Chord_Direction           : Position_Scale;
      Maximum_Lambda, Tolerance : Dimensionless;
      Warp_Factor               : Dimensionless;
      Initial                   : Closure_Solution;
      Panel_Count               : Positive := 1) return Closure_Solution
   is
      Stalled_Slope_Fraction : constant Dimensionless := 1.0E-3;
      --  A full Newton step lowers ‖R‖² at the rate -2·‖R‖². Once λ rests on a bound which the step would cross,
      --  only C₀ can move. The Gauss-Newton step in C₀ alone lowers ‖R‖² at the rate -2·‖P·R‖², where P projects
      --  onto the range of J꜀, and under the linear model no move of C₀ lowers it further than ‖R‖² - ‖P·R‖². When
      --  that rate is below this fraction of the full one, the bounded problem has reached its own minimum with the
      --  residual still above tolerance. Moving C₀ there also leaves the component of R which only λ can reduce
      --  unchanged, so the Newton step keeps pointing across the bound.

      Failed : constant Closure_Solution := (Success => False, others => <>);
      Unsafe : constant Closure_Solution := (Success => False, Numerically_Unsafe => True, others => <>);

      Closure_Frame : Frame_Vector_Array;
      Frame_OK      : Boolean;
   begin
      pragma Assert (Within (Frame (1), Unit_Component_Limit));
      pragma Assert (Within (Frame (2), Unit_Component_Limit));
      pragma Assert (Within (Frame (3), Unit_Component_Limit));
      Lemma_Within_Larger (Frame (1), Unit_Component_Limit, Candidate_Component_Limit);
      Lemma_Within_Larger (Frame (2), Unit_Component_Limit, Candidate_Component_Limit);
      Lemma_Within_Larger (Frame (3), Unit_Component_Limit, Candidate_Component_Limit);
      Frame_OK := Complete_Frame (Chord_Direction, Frame (1), Frame (2), Frame (3), Closure_Frame);
      if not Frame_OK then
         return Failed;
      end if;
      declare
         --  λ is constrained to [1, Maximum_Lambda] because a unit-speed curve cannot be shorter than its chord.
         --  Start just inside that interval unless the caller supplied a previous solution.
         --  The starting value never exceeds the largest one; taking the smaller of the two says so without an
         --  argument about rounding.
         Lambda             : Dimensionless :=
           (if Initial.Success
            then Initial.Lambda
            else Dimensionless'Min (Maximum_Lambda, 1.0 + 0.1 * Dimensionless'Min (Maximum_Lambda - 1.0, 1.0)));
         C0                 : Raw_Vector_3 := (if Initial.Success then Initial.C0 else [others => 0.0]);
         Residual, D_Lambda : Raw_Vector_4;
         Jacobian_C         : Raw_Matrix_4_3;
         Usable             : Boolean;
      begin
         pragma Assert (Lambda >= 1.0 and then Lambda <= Maximum_Lambda);
         pragma Assert (Within (C0, Tame_Limit));
         Closure_Residual_From_Terms
           (Terms,
            Frame,
            Closure_Frame,
            Chord_Direction,
            Lambda,
            C0,
            Warp_Factor,
            Residual,
            Jacobian_C,
            D_Lambda,
            Usable,
            Panel_Count => Panel_Count);
         if not Usable or else not Is_Tame_Evaluation (Residual, D_Lambda, Jacobian_C) then
            return Unsafe;
         end if;
         Iteration_Loop : for Iteration in 1 .. 20 loop
            pragma Loop_Invariant (Lambda >= 1.0 and then Lambda <= Maximum_Lambda);
            pragma Loop_Invariant (Within (C0, Tame_Limit));
            pragma Loop_Invariant (Is_Tame_Evaluation (Residual, D_Lambda, Jacobian_C));
            if Residual_Norm (Residual) <= Tolerance then
               return (Success => True, Lambda => Lambda, C0 => C0, others => <>);
            end if;
            declare
               Newton_Step : Raw_Vector_4;
               Solved      : Boolean;
            begin
               Solved := Solve_Linear (Newton_Matrix (D_Lambda, Jacobian_C), Negated (Residual), Newton_Step);
               exit Iteration_Loop when not Solved;
               if (Lambda >= Maximum_Lambda and then Newton_Step (0) > 0.0)
                 or else (Lambda <= 1.0 and then Newton_Step (0) < 0.0)
               then
                  --  The clamp below holds λ on its bound, so the trial steps move C₀ alone, and ‖R‖² changes
                  --  along them at the rate 2·R·(J꜀·ΔC₀). This is the usual way for a blend to fail: the closed
                  --  curve would be longer than the path it replaces. Recognise it here rather than after eight
                  --  backtracking evaluations which cannot make progress.
                  declare
                     Norm_Square : constant Dimensionless := Squared_Norm (Residual);
                  begin
                     if C0_Slope (Residual, Jacobian_C, Newton_Step) >= -Stalled_Slope_Fraction * Norm_Square then
                        --  The C₀ part of the full step barely descends, but another move of C₀ may. Solve the
                        --  normal equations for the Gauss-Newton step with λ held. Without a held step the full
                        --  one is backtracked as usual.
                        declare
                           Normal              : Raw_Matrix_4;
                           Gradient, Held_Step : Raw_Vector_4;
                           System_Usable       : Boolean;
                           Held_Step_Solved    : Boolean;
                        begin
                           Held_Step_System (Jacobian_C, Residual, Normal, Gradient, System_Usable);
                           if System_Usable then
                              Held_Step_Solved := Solve_Linear (Normal, Gradient, Held_Step);
                              if Held_Step_Solved then
                                 exit Iteration_Loop when
                                   C0_Slope (Residual, Jacobian_C, Held_Step) >= -Stalled_Slope_Fraction * Norm_Square;
                                 Newton_Step := Held_Step;
                              end if;
                           end if;
                        end;
                     end if;
                  end;
               end if;
               declare
                  Scale        : Dimensionless := 1.0;
                  Accepted     : Boolean := False;
                  Current_Norm : constant Dimensionless := Residual_Norm (Residual);
               begin
                  --  Backtrack Δ by powers of two until ‖R(x + α·Δ)‖ < ‖R(x)‖. Clamp λ but leave C₀
                  --  unconstrained, within the common limit, so all three closure modes remain available.
                  for Trial in 1 .. 8 loop
                     pragma Loop_Invariant (Scale >= 0.0 and then Scale <= 1.0 and then not Accepted);
                     pragma Loop_Invariant (Lambda >= 1.0 and then Lambda <= Maximum_Lambda);
                     pragma Loop_Invariant (Within (C0, Tame_Limit));
                     pragma Loop_Invariant (Is_Tame_Evaluation (Residual, D_Lambda, Jacobian_C));
                     pragma Loop_Invariant (Within (Newton_Step, Linear_Solution_Limit));
                     pragma Assert (abs Newton_Step (0) <= Linear_Solution_Limit);
                     declare
                        Trial_Lambda                   : constant Dimensionless :=
                          Dimensionless'Max
                            (1.0, Dimensionless'Min (Maximum_Lambda, Lambda + Scale * Newton_Step (0)));
                        Trial_C0                       : constant Raw_Vector_3 :=
                          Trial_Coefficients (C0, Newton_Step, Scale);
                        Trial_Residual, Trial_D_Lambda : Raw_Vector_4;
                        Trial_Jacobian                 : Raw_Matrix_4_3;
                        Trial_Usable                   : Boolean;
                     begin
                        --  A step which takes a closure coefficient beyond the limit is not one to accept.
                        if Within (Trial_C0, Tame_Limit) then
                           Closure_Residual_From_Terms
                             (Terms,
                              Frame,
                              Closure_Frame,
                              Chord_Direction,
                              Trial_Lambda,
                              Trial_C0,
                              Warp_Factor,
                              Trial_Residual,
                              Trial_Jacobian,
                              Trial_D_Lambda,
                              Trial_Usable,
                              Panel_Count => Panel_Count);
                           if not Trial_Usable then
                              return Unsafe;
                           end if;
                           if Is_Tame_Evaluation (Trial_Residual, Trial_D_Lambda, Trial_Jacobian)
                             and then Residual_Norm (Trial_Residual) < Current_Norm
                           then
                              Lambda := Trial_Lambda;
                              C0 := Trial_C0;
                              Residual := Trial_Residual;
                              D_Lambda := Trial_D_Lambda;
                              Jacobian_C := Trial_Jacobian;
                              Accepted := True;
                              exit;
                           end if;
                        end if;
                     end;
                     Scale := 0.5 * Scale;
                  end loop;
                  exit Iteration_Loop when not Accepted;
               end;
            end;
         end loop Iteration_Loop;
      end;
      return Failed;
   end Solve_Closure_From_Terms;

   ----------------------------------------------------------------------------------------------------------------
   --  Outward-rounded bounds for the retained realtime tangent
   --
   --  The pole-based Taylor enclosure is the primary path for every range. The rational Bernstein certificate is its
   --  fallback for a range where a pole lies too close for the enclosure's fixed work, or where the partial fractions
   --  cancel too far for its rounding allowance.
   ----------------------------------------------------------------------------------------------------------------

   --  Enclose division by an interval that excludes zero.

   Quotient_Operand_Limit : constant := 2.0**500;
   Quotient_Divisor_Floor : constant := 2.0**(-500);
   --  The largest magnitude of an endpoint of a numerator, and the smallest of an endpoint of a denominator, for
   --  which Interval_Divide forms the quotient.

   function Interval_Divide (Numerator, Denominator : Interval) return Interval is
   begin
      if not Numerator.Valid
        or else not Denominator.Valid
        or else (Denominator.Lower <= 0.0 and then Denominator.Upper >= 0.0)
      then
         return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      elsif Numerator.Lower = 0.0 and then Numerator.Upper = 0.0 then
         return Interval_Exact (0.0);
      elsif not (abs Numerator.Lower <= Quotient_Operand_Limit)
        or else not (abs Numerator.Upper <= Quotient_Operand_Limit)
        or else not (abs Denominator.Lower >= Quotient_Divisor_Floor)
        or else not (abs Denominator.Upper >= Quotient_Divisor_Floor)
      then
         --  A quotient of endpoints beyond these limits could overflow, so it is not formed. No enclosure which
         --  a certificate uses comes near them.
         return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      end if;
      declare
         Q1 : constant Dimensionless := Numerator.Lower / Denominator.Lower;
         Q2 : constant Dimensionless := Numerator.Lower / Denominator.Upper;
         Q3 : constant Dimensionless := Numerator.Upper / Denominator.Lower;
         Q4 : constant Dimensionless := Numerator.Upper / Denominator.Upper;
      begin
         return
           Checked_Interval
             (Down (Dimensionless'Min (Dimensionless'Min (Q1, Q2), Dimensionless'Min (Q3, Q4))),
              Up (Dimensionless'Max (Dimensionless'Max (Q1, Q2), Dimensionless'Max (Q3, Q4))));
      end;
   end Interval_Divide;

   function Divide_By_Positive (Numerator, Denominator : Interval) return Interval;

   function Divide_By_Positive (Numerator, Denominator : Interval) return Interval is
   begin
      if not Denominator.Valid or else Denominator.Lower <= 0.0 then
         return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      end if;
      return Interval_Divide (Numerator, Denominator);
   end Divide_By_Positive;

   type Interval_Complex is record
      Real_Part      : Interval;
      Imaginary_Part : Interval;
   end record;
   function Complex_Multiply (Left, Right : Interval_Complex) return Interval_Complex;
   --  Return an interval enclosure of the product of two complex intervals.

   function Complex_Multiply (Left, Right : Interval_Complex) return Interval_Complex is
   begin
      return
        (Real_Part      =>
           Interval_Subtract
             (Interval_Multiply (Left.Real_Part, Right.Real_Part),
              Interval_Multiply (Left.Imaginary_Part, Right.Imaginary_Part)),
         Imaginary_Part =>
           Interval_Add
             (Interval_Multiply (Left.Real_Part, Right.Imaginary_Part),
              Interval_Multiply (Left.Imaginary_Part, Right.Real_Part)));
   end Complex_Multiply;
   function Complex_Reciprocal (U, Pole_Real, Pole_Imaginary : Dimensionless) return Interval_Complex;
   --  Enclose the reciprocal of U minus the supplied complex pole.

   function Complex_Reciprocal (U, Pole_Real, Pole_Imaginary : Dimensionless) return Interval_Complex is
      Real_Difference  : constant Interval := Interval_Subtract (Interval_Exact (U), Interval_Exact (Pole_Real));
      Imaginary        : constant Interval := Interval_Exact (Pole_Imaginary);
      Real_Square      : Interval := Interval_Multiply (Real_Difference, Real_Difference);
      Imaginary_Square : Interval := Interval_Multiply (Imaginary, Imaginary);
      Denominator      : Interval;
   begin
      --  Dependency can give a slightly negative lower endpoint for X*X. The analytic square is nonnegative.
      Real_Square.Lower := Dimensionless'Max (0.0, Real_Square.Lower);
      Imaginary_Square.Lower := Dimensionless'Max (0.0, Imaginary_Square.Lower);
      Denominator := Interval_Add (Real_Square, Imaginary_Square);
      Denominator.Lower := Dimensionless'Max (0.0, Denominator.Lower);
      return
        (Real_Part      => Divide_By_Positive (Real_Difference, Denominator),
         Imaginary_Part => Divide_By_Positive (Imaginary, Denominator));
   end Complex_Reciprocal;

   function Lower_Power (Base : Dimensionless; Exponent : Positive) return Dimensionless
   with Post => Lower_Power'Result >= 0.0 and then Lower_Power'Result <= 2.0**400;
   --  Compute a downward-rounded positive integer power.

   Lower_Power_Limit : constant := 2.0**400;
   --  Largest base and largest partial product of Lower_Power.

   function Lower_Power (Base : Dimensionless; Exponent : Positive) return Dimensionless is
      Value : Dimensionless := 1.0;
   begin
      --  Zero is a lower bound of every power of a positive base. It is returned where a product could leave the
      --  range in which the next one is formed, as it is for a base which is not positive.
      if not (Base > 0.0 and then Base <= Lower_Power_Limit) then
         return 0.0;
      end if;
      for I in 1 .. Exponent loop
         pragma Loop_Invariant (Value > 0.0 and then Value <= Lower_Power_Limit);
         Value := Down (Value * Base);
         if not (Value > 0.0 and then Value <= Lower_Power_Limit) then
            return 0.0;
         end if;
      end loop;
      return Value;
   end Lower_Power;

   function Elevate_Bernstein
     (Source : Interval_Polynomial; Source_Degree, Target_Degree : Natural) return Interval_Polynomial;
   pragma Annotate (GNATprove, Skip_Proof, Elevate_Bernstein);
   --  Not proved: this recovers from overflow by handling Constraint_Error, or calls what does.
   --  Elevate a Bernstein polynomial to Target_Degree without changing its value.

   function Elevate_Bernstein
     (Source : Interval_Polynomial; Source_Degree, Target_Degree : Natural) return Interval_Polynomial
   is
      Result : Interval_Polynomial (Source'Range) := [others => Interval_Exact (0.0)];
   begin
      if Source_Degree > Target_Degree or else Target_Degree > Source'Last then
         return [Source'Range => (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False)];
      elsif Source_Degree = Target_Degree then
         return Source;
      end if;
      Result (0 .. Target_Degree) :=
        Multiply_Bernstein
          (Source (0 .. Source_Degree),
           Interval_Polynomial'(0 .. Target_Degree - Source_Degree => Interval_Exact (1.0)));
      return Result;
   exception
      when Constraint_Error =>
         return [Source'Range => (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False)];
   end Elevate_Bernstein;

   subtype Correction_Order is Natural range 0 .. Maximum_Majorant_Order + 2;
   subtype Correction_Controls is Interval_Polynomial (0 .. 11);
   type Correction_Control_Table is array (Correction_Order) of Correction_Controls;

   function Build_Correction_Controls return Correction_Control_Table;
   --  Enclosures of the Bernstein controls of the finish correction's derivatives, each divided by the factorial
   --  of its order.

   function Build_Correction_Controls return Correction_Control_Table is
      Result : Correction_Control_Table;
   begin
      for Order in Correction_Order loop
         declare
            Work   : Correction_Controls := [for Index in 0 .. 11 => Interval_Exact (if Index <= 5 then 0.0 else 1.0)];
            Degree : Natural := 11;
         begin
            for Derivative in 1 .. Order + 1 loop
               pragma Loop_Invariant (Degree = 12 - Derivative);
               for Index in 0 .. Degree - 1 loop
                  Work (Index) :=
                    Interval_Multiply
                      (Interval_Exact (Dimensionless (Degree)), Interval_Subtract (Work (Index + 1), Work (Index)));
               end loop;
               Degree := Degree - 1;
            end loop;
            if Order > 1 then
               for Index in 0 .. Degree loop
                  Work (Index) :=
                    Interval_Multiply
                      (Work (Index), Checked_Interval (Down (1.0 / Factorial (Order)), Up (1.0 / Factorial (Order))));
               end loop;
            end if;
            Result (Order) := Work;
         end;
      end loop;
      return Result;
   end Build_Correction_Controls;

   Correction_Derivatives : constant Correction_Control_Table := Build_Correction_Controls;

   function Correction_Hull_Bound (Value : Correction_Controls; Degree, Depth : Natural) return Dimensionless
   with Pre => Degree <= Correction_Controls'Last, Subprogram_Variant => (Decreases => Depth);
   --  A bound on the magnitude of a polynomial on the unit interval from its Bernstein controls, tightened by
   --  Depth bisections. Dimensionless'Last, which is no bound, if a control is beyond the largest number halved.

   function Correction_Hull_Bound (Value : Correction_Controls; Degree, Depth : Natural) return Dimensionless is
      Left  : Correction_Controls := [others => Interval_Exact (0.0)];
      Right : Correction_Controls := Value;
      Bound : Dimensionless := 0.0;
   begin
      if Depth = 0 then
         for Index in 0 .. Degree loop
            Bound := Dimensionless'Max (Bound, Interval_Abs_Max (Value (Index)));
         end loop;
         return Up (Bound);
      end if;
      Left (0) := Right (0);
      for Level in 1 .. Degree loop
         for Index in 0 .. Degree - Level loop
            if Right (Index).Lower /= Right (Index + 1).Lower or else Right (Index).Upper /= Right (Index + 1).Upper
            then
               Right (Index).Lower := 0.5 * Right (Index).Lower + 0.5 * Right (Index + 1).Lower;
               Right (Index).Upper := 0.5 * Right (Index).Upper + 0.5 * Right (Index + 1).Upper;
               --  The controls of the correction are small integers and their averages, so this never fails.
               if not (abs Right (Index).Lower <= Dimensionless'Last / 2.0)
                 or else not (abs Right (Index).Upper <= Dimensionless'Last / 2.0)
               then
                  return Dimensionless'Last;
               end if;
               Inflate_Finite_Endpoints (Right (Index).Lower, Right (Index).Upper);
            end if;
         end loop;
         Left (Level) := Right (0);
      end loop;
      return
        Dimensionless'Max
          (Correction_Hull_Bound (Left, Degree, Depth - 1), Correction_Hull_Bound (Right, Degree, Depth - 1));
   end Correction_Hull_Bound;

   Full_Correction_Bounds : constant array (Correction_Order) of Dimensionless :=
     [for Order in Correction_Order => Correction_Hull_Bound (Correction_Derivatives (Order), 10 - Order, 4)];

   function Pole_Taylor_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless; Sharp : out Boolean) return Axis_Majorants
   is
      --  The retained evaluator's tangent is, in normalized distance U,
      --
      --     T(U) = c + Σ r/(U - p) + Σ 2·Re(ρ/(U - π)) + (C/L)·H′(U),
      --
      --  with real poles p, conjugate pairs π, the endpoint correction displacement C and the smootherstep H. The
      --  public bounds need A_0 through A_4 for the rational part, where A_n(U) = T⁽ⁿ⁾(U)/n!:
      --
      --     A_n(U) = [n = 0]·c + (-1)ⁿ·(Σ r·(U - p)⁻⁽ⁿ⁺¹⁾ + Σ 2·Re(ρ·(U - π)⁻⁽ⁿ⁺¹⁾)).
      --
      --  On a cell |U - m| ≤ h the Taylor expansion of every pole term about m gives, for any N ≥ 1,
      --
      --     A_k(m + t) = Σ_{j<N} C(k+j, j)·A_{k+j}(m)·tʲ + R,   |R| ≤ C(k+N, N)·hᴺ·Σ |r|/dᵏ⁺ᴺ⁺¹,
      --
      --  where d is each pole's distance from the cell: the integral form of the remainder of (z + t)⁻⁽ᵏ⁺¹⁾ is
      --  bounded by |t|ᴺ times the largest magnitude of its Nth Taylor coefficient C(k+N, N)·(z + τ)⁻⁽ᵏ⁺ᴺ⁺¹⁾ on the
      --  cell. The first three terms are maximized exactly as a quadratic, the next three by their absolute values,
      --  and only the sixth-order remainder falls back to the sum of absolute residues. That sum can exceed the
      --  true derivative by orders of magnitude when partial fractions cancel, which the factor h⁶ absorbs. The
      --  same expansion bounds the cell's maximum from below, by the quadratic's maximum less everything added to
      --  it. Cells whose upper bound is not yet close to the largest such lower bound are trisected, so the work
      --  concentrates on the few cells which contain an extremum.
      --
      --  All of this is ordinary rounded arithmetic with explicit allowances rather than interval arithmetic:
      --
      --    * Every centre m and pole component is an exact stored number, so x = m - A has relative error at most
      --      u = 2⁻⁵³. The denominator x² + B² adds positive terms, so it is correct to 4u and the reciprocal
      --      w = (x + iB)/(x² + B²) to 6u. A rounded complex product errs by at most 2u·(|ac| + |bd|) ≤ 2u·|P|·|w|
      --      in each part, 2.83u in modulus, and multiplies by the rounded w again; hence wⁿ formed one factor at
      --      a time is correct to 6u + 8.83u·(n - 1), which is 86u for the largest power n = 10. The powers are
      --      formed two factors at a time instead: w² is correct to 14.83u and each product by it adds 17.66u,
      --      which gives 6u + 17.66u·m for w²ᵐ⁺¹ and 14.83u + 17.66u·m for w²ᵐ⁺², never more than the single-factor
      --      bound and again 86u for n = 10. Forming Re(r·wⁿ) adds 2u, so one
      --      term of A_n differs from its exact value by at most 88u·(|R| + |I|)·|w|ⁿ⁺¹. Seventeen terms accumulate
      --      at most 16u more, relative to the sum of their magnitudes. Round_Off = 2⁻⁴⁵ = 256u is more than twice
      --      that total of 104u, and the sum of magnitudes at the centre is no larger than the cell sum
      --      Σ |r|/dⁿ⁺¹ computed for the remainder. A fused multiply-add only reduces these errors.
      --    * Distances are rounded toward zero and the sums of positive terms are inflated by Round_Off, far more
      --      than their few-ulp error.
      --    * Rounded centres and radii can leave gaps between neighbouring cells. The base partition errs by at
      --      most 4u and each trisection by 2u more, 16u in all; every radius is padded by Cell_Pad = 64u. A
      --      range consisting of one exact point needs no padding.
      --    * The preflight bounds every stored magnitude by Magnitude_Limit and the distance of every pole from
      --      [0, 1] below by twice Minimum_Distance, so no product can overflow: |w|¹⁰·|r| and |r|/d¹¹ stay below
      --      10¹²⁰·10⁶¹ and 10¹³²·10⁶¹. Overflow checks are therefore suppressed in the kernels.
      --
      --  The allowance for rounding is proportional to the sum of absolute partial-fraction terms and does not
      --  shrink with the cell. Where those terms cancel to more than eleven digits, as on a range where the curve
      --  is almost straight, it becomes a visible part of the bound. Sharp reports whether every bound is free of
      --  that effect, and of any slack which the work limits left unresolved; otherwise the Bernstein form, whose
      --  controls are small wherever the function is small, may do better.
      Model_Order       : constant := 6;
      Maximum_Jet_Order : constant := Maximum_Majorant_Order + Model_Order - 1;
      Maximum_Sum_Order : constant := Maximum_Majorant_Order + Model_Order;
      subtype Jet_Order is Natural range 0 .. Maximum_Jet_Order;
      subtype Sum_Order is Natural range 0 .. Maximum_Sum_Order;

      Base_Cells          : constant := 8;
      Maximum_Depth       : constant := 6;
      Maximum_Evaluations : constant := 128;
      --  Trisection keeps the parent's centre, so each refinement costs two evaluations. Typical curves need about
      --  twenty-five and strongly warped ones under a hundred. The cap bounds the work, and the size of the cell
      --  queue on the stack, when cancellation is extreme; cells left unrefined keep their valid but looser bound.

      Tightness_Factor : constant Dimensionless := 1.005;
      Tightness_Floor  : constant Dimensionless := 1.0E-12;
      --  A cell is accepted once its bound is within half a percent of a magnitude which the function is known to
      --  attain: at an evaluated centre, or anywhere in a cell according to that cell's own model less its slack.

      Round_Off         : constant Dimensionless := 2.0**(-45);
      Cell_Pad          : constant Dimensionless := 2.0**(-47);
      Noise_Fraction    : constant Dimensionless := 0.02;
      Noise_Floor       : constant Dimensionless := 1.0E-9;
      --  A bound is sharp while its rounding allowance is at most this fraction of it, or is below a magnitude
      --  which the Bernstein form cannot resolve either.
      Loose_Fraction    : constant Dimensionless := 0.02;
      --  A bound is also sharp while no cell which the work limits left loose exceeds a magnitude known to be
      --  attained by more than this fraction, the Bernstein form's own tolerance, or by Noise_Floor.
      Magnitude_Limit   : constant Dimensionless := 1.0E60;
      Minimum_Distance  : constant Dimensionless := 1.0E-12;
      Underflow_Padding : constant Dimensionless := 2.0**(-500);
      --  The relative allowances above assume no underflow. An underflowing operation errs by less than 2⁻¹⁰⁷⁵
      --  instead, which at most nine later multiplications by |w| < 2⁴⁰ and one by |r| < 2²⁰¹ amplify to less than
      --  2⁻⁵¹⁴. Fewer than 2¹⁰ operations feed one sum, so this padding covers them all. An axis without any residue
      --  receives none: its sums are exact zeros, which keeps an algebraically constant tangent's bounds at zero.

      subtype Active_Index is Positive range 1 .. Axis_Name'Pos (Axis_Name'Last) - Axis_Name'Pos (Axis_Name'First) + 1;
      type Active_Values is array (Active_Index) of Dimensionless;
      type Jet_Array is array (Active_Index, Jet_Order) of Dimensionless;
      type Sum_Array is array (Active_Index, Sum_Order) of Dimensionless;
      type Order_Bounds is array (Active_Index, Majorant_Order) of Dimensionless;

      type Pole_Term is record
         Real_Part                       : Dimensionless := 0.0;
         Imaginary_Part                  : Dimensionless := 0.0;
         --  Zero for a real pole.
         Residue_Real, Residue_Imaginary : Active_Values := [others => 0.0];
         --  A pair's residue is stored doubled, which is exact.
         Magnitude                       : Active_Values := [others => 0.0];
         --  Upper bound on the modulus of the stored residue.
      end record;

      type Cell is record
         Centre, Radius : Dimensionless;
         Depth          : Natural;
         Jets           : Jet_Array;
      end record;

      Queue_Capacity : constant := Base_Cells + 3 * ((Maximum_Evaluations - Base_Cells) / 2);

      Cache      : Rational_Antiderivative renames Curve.Evaluator_Data.Antiderivative_Cache;
      Length_Raw : constant Dimensionless := Dimensionless (Curve.Evaluator_Data.Length_Value / mm);
      Saturated  : constant Axis_Majorants := [others => [others => Dimensionless'Last]];

      Active_Axes  : array (Active_Index) of Axis_Name := [others => Axis_Name'First];
      Active_Count : Natural := 0;
      Gathered     : Boolean;
      Constants    : Active_Values := [others => 0.0];
      Padding      : Active_Values := [others => 0.0];
      Terms        : array (Rational_Degree_Slot) of Pole_Term;
      Term_Count   : Natural := 0;

      type Weight_Table is array (Majorant_Order, 0 .. Model_Order) of Dimensionless;
      Taylor_Weights : constant Weight_Table :=
        [for Order in Majorant_Order => [for Term in 0 .. Model_Order => Binomial (Order + Term, Term)]];
      --  C(k+j, j), the coefficient of A_{k+j}·tʲ in the expansion of A_k.

      Range_Sums : Sum_Array;
      --  The sums for the complete padded range. They are valid, if larger than necessary, for every cell.

      Queue       : array (1 .. Queue_Capacity) of Cell
      with Relaxed_Initialization;
      --  Only the cells from Head to Tail are read, and every one of them has been written in full.
      Head        : Positive := 1;
      Tail        : Natural := 0;
      Evaluations : Natural := 0;
      Lower       : Order_Bounds := [others => [others => 0.0]];
      --  Magnitudes known to be attained somewhere in the range. They only steer refinement, so they need not be
      --  rigorous.
      Rational    : Order_Bounds := [others => [others => 0.0]];
      Noise       : Order_Bounds := [others => [others => 0.0]];
      --  The rounding allowance included in the corresponding entry of Rational.
      Unresolved  : Order_Bounds := [others => [others => 0.0]];
      --  The largest bound, less twice its rounding allowance, of any cell accepted while still loose because the
      --  depth, evaluation or queue limit stopped its refinement.
      Pad         : constant Dimensionless := (if Start_U = End_U then 0.0 else Cell_Pad);
      Result      : Axis_Majorants := [others => [others => 0.0]];

      Correction_Bounds       : array (Majorant_Order) of Dimensionless := [others => 0.0];
      Correction_Bounds_Known : Boolean := False;
      --  Correction_Bound for every order, formed on first use. It depends on the range but not on the axis.

      function Correction_Coefficient (Order : Majorant_Order; U : Dimensionless) return Interval;
      --  Enclose the factorial-scaled smootherstep derivative H⁽ᴼʳᵈᵉʳ⁺¹⁾(U)/Order!.

      function Correction_Coefficient (Order : Majorant_Order; U : Dimensionless) return Interval is
         Work        : Correction_Controls := Correction_Derivatives (Order);
         Degree      : constant Natural := 10 - Order;
         T           : constant Interval := Interval_Exact (U);
         One_Minus_T : constant Interval := Interval_Subtract (Interval_Exact (1.0), T);
      begin
         for Level in 1 .. Degree loop
            for Index in 0 .. Degree - Level loop
               Work (Index) :=
                 Interval_Add (Interval_Multiply (One_Minus_T, Work (Index)), Interval_Multiply (T, Work (Index + 1)));
            end loop;
         end loop;
         return Work (0);
      exception
         when Constraint_Error =>
            return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      end Correction_Coefficient;

      function Correction_Bound (Order : Majorant_Order) return Dimensionless;
      --  Bound the same coefficient over the requested range.

      function Correction_Bound (Order : Majorant_Order) return Dimensionless is
      begin
         if Start_U <= 0.0 and then End_U >= 1.0 then
            return Full_Correction_Bounds (Order);
         end if;
         --  The derivative of the coefficient of order n is (n + 1) times the coefficient of order n + 1, so the
         --  mean value theorem bounds the range by the midpoint value plus the radius times that slope.
         declare
            Middle : constant Dimensionless := 0.5 * Start_U + 0.5 * End_U;
            Radius : constant Dimensionless := Up (Dimensionless'Max (abs (Middle - Start_U), abs (End_U - Middle)));
            Value  : constant Interval := Correction_Coefficient (Order, Middle);
            Slope  : constant Dimensionless := Up (Dimensionless (Order + 1) * Full_Correction_Bounds (Order + 1));
         begin
            if not Value.Valid then
               return Full_Correction_Bounds (Order);
            end if;
            return
              Dimensionless'Min (Full_Correction_Bounds (Order), Up (Interval_Abs_Max (Value) + Up (Radius * Slope)));
         end;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Correction_Bound;

      function Gather_Terms return Boolean
      with Side_Effects;
      --  Copy the active axes' poles and residues into Terms. False if a magnitude or a pole distance is outside
      --  the range for which the kernels below cannot overflow.

      function Gather_Terms return Boolean is
         function Usable (Value : Dimensionless) return Boolean
         is (abs Value <= Magnitude_Limit);
         --  False for NaN as well.

         procedure Append (Real_Slot, Imaginary_Slot : Natural; Usable_Term : out Boolean);

         procedure Append (Real_Slot, Imaginary_Slot : Natural; Usable_Term : out Boolean) is
            Term   : Pole_Term;
            Factor : constant Dimensionless := (if Imaginary_Slot = 0 then 1.0 else 2.0);
         begin
            Term.Real_Part := Cache.Pole_Slots (Real_Slot).Pole_Component;
            Term.Imaginary_Part :=
              (if Imaginary_Slot = 0 then 0.0 else Cache.Pole_Slots (Imaginary_Slot).Pole_Component);
            Usable_Term := Usable (Term.Real_Part) and then Usable (Term.Imaginary_Part);
            if not Usable_Term then
               return;
            end if;
            declare
               --  Every evaluation point lies in [0, 1], up to the cell padding.
               Horizontal : constant Dimensionless :=
                 (if Term.Real_Part < 0.0
                  then -Term.Real_Part
                  elsif Term.Real_Part > 1.0
                  then Term.Real_Part - 1.0
                  else 0.0);
            begin
               if Imaginary_Slot = 0 then
                  Usable_Term := Horizontal >= 2.0 * Minimum_Distance;
               else
                  Usable_Term :=
                    Term.Imaginary_Part > 0.0
                    and then Dimensionless'Max (Horizontal, Term.Imaginary_Part) >= 2.0 * Minimum_Distance;
               end if;
            end;
            for Index in 1 .. Active_Count loop
               declare
                  Real_Residue      : constant Dimensionless :=
                    Cache.Pole_Slots (Real_Slot).Residue_Component (Active_Axes (Index));
                  Imaginary_Residue : constant Dimensionless :=
                    (if Imaginary_Slot = 0
                     then 0.0
                     else Cache.Pole_Slots (Imaginary_Slot).Residue_Component (Active_Axes (Index)));
               begin
                  Usable_Term := Usable_Term and then Usable (Real_Residue) and then Usable (Imaginary_Residue);
                  exit when not Usable_Term;
                  Term.Residue_Real (Index) := Factor * Real_Residue;
                  Term.Residue_Imaginary (Index) := Factor * Imaginary_Residue;
                  --  |R + iI| ≤ |R| + |I|; the sum is rounded upward.
                  Term.Magnitude (Index) := Up (abs Term.Residue_Real (Index) + abs Term.Residue_Imaginary (Index));
                  if Term.Magnitude (Index) > 0.0 then
                     Padding (Index) := Underflow_Padding;
                  end if;
               end;
            end loop;
            if Usable_Term then
               Term_Count := Term_Count + 1;
               Terms (Term_Count) := Term;
            end if;
         end Append;

         Good : Boolean;
      begin
         for Index in 1 .. Active_Count loop
            Constants (Index) := Cache.Constant_Tangent (Active_Axes (Index));
            if not Usable (Constants (Index)) then
               return False;
            end if;
         end loop;
         for Index in 1 .. Cache.Real_Pole_Count loop
            Append (Index, 0, Good);
            if not Good then
               return False;
            end if;
         end loop;
         for Pair in 1 .. Cache.Pair_Count loop
            Append
              (Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair)),
               Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair)),
               Good);
            if not Good then
               return False;
            end if;
         end loop;
         return True;
      end Gather_Terms;

      procedure Evaluate_Jets (Centre : Dimensionless; Jets : out Jet_Array);
      --  Compute A_0 through A_9 of the rational part at Centre for every active axis, and raise Lower.

      procedure Evaluate_Jets (Centre : Dimensionless; Jets : out Jet_Array) is
         pragma Suppress (Overflow_Check);
         Power_Real, Power_Imag : array (Jet_Order) of Dimensionless := [others => 0.0];
      begin
         Jets := [others => [others => 0.0]];
         for Term of Terms (1 .. Term_Count) loop
            declare
               X : constant Dimensionless := Centre - Term.Real_Part;
            begin
               --  Form the powers of 1/(z - p) first, signed by order, so that each axis then accumulates them in
               --  one straight pass.
               if Term.Imaginary_Part = 0.0 then
                  declare
                     Reciprocal : constant Dimensionless := 1.0 / X;
                     Power      : Dimensionless := Reciprocal;
                  begin
                     for Order in Jet_Order loop
                        Power_Real (Order) := Power;
                        Power := -(Power * Reciprocal);
                     end loop;
                  end;
                  for Index in 1 .. Active_Count loop
                     for Order in Jet_Order loop
                        Jets (Index, Order) := Jets (Index, Order) + Term.Residue_Real (Index) * Power_Real (Order);
                     end loop;
                  end loop;
               else
                  declare
                     Square          : constant Dimensionless := X * X + Term.Imaginary_Part * Term.Imaginary_Part;
                     Reciprocal_Real : constant Dimensionless := X / Square;
                     Reciprocal_Imag : constant Dimensionless := Term.Imaginary_Part / Square;
                     --  w², by which each of two interleaved chains advances: the powers of even and of odd order
                     --  then do not wait for one another.
                     Square_Real     : constant Dimensionless :=
                       Reciprocal_Real * Reciprocal_Real - Reciprocal_Imag * Reciprocal_Imag;
                     Square_Imag     : constant Dimensionless := 2.0 * (Reciprocal_Real * Reciprocal_Imag);
                  begin
                     Power_Real (0) := Reciprocal_Real;
                     Power_Imag (0) := Reciprocal_Imag;
                     Power_Real (1) := -Square_Real;
                     Power_Imag (1) := -Square_Imag;
                     for Order in 2 .. Jet_Order'Last loop
                        Power_Real (Order) :=
                          Power_Real (Order - 2) * Square_Real - Power_Imag (Order - 2) * Square_Imag;
                        Power_Imag (Order) :=
                          Power_Real (Order - 2) * Square_Imag + Power_Imag (Order - 2) * Square_Real;
                     end loop;
                  end;
                  for Index in 1 .. Active_Count loop
                     for Order in Jet_Order loop
                        Jets (Index, Order) :=
                          Jets (Index, Order)
                          + (Term.Residue_Real (Index) * Power_Real (Order)
                             - Term.Residue_Imaginary (Index) * Power_Imag (Order));
                     end loop;
                  end loop;
               end if;
            end;
         end loop;
         for Index in 1 .. Active_Count loop
            Jets (Index, 0) := Jets (Index, 0) + Constants (Index);
            for Order in Majorant_Order loop
               if abs Jets (Index, Order) > Lower (Index, Order) then
                  Lower (Index, Order) := abs Jets (Index, Order);
               end if;
            end loop;
         end loop;
         Evaluations := Evaluations + 1;
      end Evaluate_Jets;

      procedure Evaluate_Sums (Centre, Radius : Dimensionless; Sums : out Sum_Array; Bounded : out Boolean);
      --  Compute upper bounds on Σ |r|/dⁿ⁺¹ for n = 0 .. 10, where d is each pole's distance from the cell of
      --  the given padded Radius. Bounded is False if a pole is too close to the cell for a finite bound.

      procedure Evaluate_Sums (Centre, Radius : Dimensionless; Sums : out Sum_Array; Bounded : out Boolean) is
         pragma Suppress (Overflow_Check);
         Powers : array (Sum_Order) of Dimensionless;
      begin
         Sums := [others => [others => 0.0]];
         Bounded := True;
         for Term of Terms (1 .. Term_Count) loop
            declare
               Offset     : constant Dimensionless := abs (Centre - Term.Real_Part);
               --  The subtraction of two exact numbers and the following difference each err by at most one unit
               --  in the last place of Offset; 2⁻⁴⁹·Offset covers both.
               Horizontal : constant Dimensionless := (Offset - Radius) - 2.0**(-49) * Offset;
               Distance   : Dimensionless;
            begin
               if Term.Imaginary_Part = 0.0 then
                  Distance := Horizontal;
               elsif Horizontal <= 0.0 then
                  Distance := Term.Imaginary_Part;
               else
                  Distance :=
                    Dimensionless_Math.Sqrt (Horizontal * Horizontal + Term.Imaginary_Part * Term.Imaginary_Part)
                    * (1.0 - 2.0**(-50));
               end if;
               if not (Distance >= Minimum_Distance) then
                  Bounded := False;
                  return;
               end if;
               declare
                  Reciprocal : constant Dimensionless := (1.0 / Distance) * (1.0 + 2.0**(-50));
                  Power      : Dimensionless := Reciprocal;
               begin
                  for Order in Sum_Order loop
                     Powers (Order) := Power;
                     Power := Power * Reciprocal;
                  end loop;
               end;
               for Index in 1 .. Active_Count loop
                  for Order in Sum_Order loop
                     Sums (Index, Order) := Sums (Index, Order) + Term.Magnitude (Index) * Powers (Order);
                  end loop;
               end loop;
            end;
         end loop;
         for Index in 1 .. Active_Count loop
            Sums (Index, 0) := Sums (Index, 0) + abs Constants (Index);
            for Order in Sum_Order loop
               Sums (Index, Order) := Sums (Index, Order) * (1.0 + Round_Off) + Padding (Index);
            end loop;
         end loop;
      end Evaluate_Sums;

      procedure Bound_Cell
        (Item               : Cell;
         Radius             : Dimensionless;
         Sums               : Sum_Array;
         Bounds, Allowances : out Order_Bounds;
         Refine             : out Boolean);
      --  Bound every active axis and order on one cell of the given padded Radius, using sums which are valid for
      --  a set containing that cell, and report the rounding allowance within each bound. Refine reports whether
      --  some bound is still loose.

      procedure Bound_Cell
        (Item               : Cell;
         Radius             : Dimensionless;
         Sums               : Sum_Array;
         Bounds, Allowances : out Order_Bounds;
         Refine             : out Boolean)
      is
         pragma Suppress (Overflow_Check);
         Powers : array (0 .. Model_Order) of Dimensionless := [others => 0.0];
         Scales : array (Majorant_Order, 1 .. Model_Order) of Dimensionless;
         --  C(k+j, j)·hʲ, the same for every axis.
      begin
         Refine := False;
         --  Only the active axes are bounded below. The remaining entries are never read, but an out parameter is
         --  left wholly defined.
         Bounds := [others => [others => 0.0]];
         Allowances := [others => [others => 0.0]];
         Powers (0) := 1.0;
         --  The loop invariants of Bound_Cell and of the enclosing function's statements stop GNATprove from
         --  unrolling their loops, which it would otherwise repeat at each inlined call of Bound_Cell. Unrolled, the
         --  proof of the enclosing function uses 9 GB of memory.
         for Exponent in 1 .. Model_Order loop
            pragma Loop_Invariant (True);
            Powers (Exponent) := Powers (Exponent - 1) * Radius;
         end loop;
         for Order in Majorant_Order loop
            pragma Loop_Invariant (True);
            for Term in 1 .. Model_Order loop
               pragma Loop_Invariant (True);
               Scales (Order, Term) := Taylor_Weights (Order, Term) * Powers (Term);
            end loop;
         end loop;
         for Index in 1 .. Active_Count loop
            for Order in Majorant_Order loop
               pragma Loop_Invariant (True);
               declare
                  A0        : constant Dimensionless := Item.Jets (Index, Order);
                  A1        : constant Dimensionless := Scales (Order, 1) * Item.Jets (Index, Order + 1);
                  A2        : constant Dimensionless := Scales (Order, 2) * Item.Jets (Index, Order + 2);
                  --  max over |t| ≤ 1 of |A0 + A1·t + A2·t²|: an endpoint, or the vertex when it is interior.
                  Quadratic : Dimensionless := abs (A0 + A2) + abs A1;
                  Rounding  : Dimensionless :=
                    Sums (Index, Order) + Scales (Order, 1) * Sums (Index, Order + 1)
                    + Scales (Order, 2) * Sums (Index, Order + 2);
                  Tail      : Dimensionless := 0.0;
                  Bound     : Dimensionless;
               begin
                  if A2 /= 0.0 and then abs A1 < 2.0 * abs A2 then
                     Quadratic := Dimensionless'Max (Quadratic, abs (A0 - A1 * A1 / (4.0 * A2)));
                  end if;
                  for Term in 3 .. Model_Order - 1 loop
                     pragma Loop_Invariant (True);
                     Tail := Tail + Scales (Order, Term) * abs Item.Jets (Index, Order + Term);
                     Rounding := Rounding + Scales (Order, Term) * Sums (Index, Order + Term);
                  end loop;
                  --  Rounding is the allowance for the computed jets; the last term is the Taylor remainder. The
                  --  final factor covers the rounding of this expression, whose terms are all nonnegative and
                  --  whose quadratic part errs by at most 16u of its value.
                  Tail :=
                    Tail + Round_Off * Rounding + Scales (Order, Model_Order) * Sums (Index, Order + Model_Order);
                  Bound := (Quadratic + Tail) * (1.0 + Round_Off) + Padding (Index);
                  Bounds (Index, Order) := Bound;
                  Allowances (Index, Order) := Round_Off * Rounding;
                  --  The function comes within Tail of the quadratic's maximum somewhere in this cell.
                  if Quadratic - Tail > Lower (Index, Order) then
                     Lower (Index, Order) := Quadratic - Tail;
                  end if;
                  --  The rounding allowance does not shrink with the cell, so it is excluded from the decision.
                  if Bound
                    > Tightness_Factor * Lower (Index, Order) + Tightness_Floor + 2.0 * Allowances (Index, Order)
                  then
                     Refine := True;
                  end if;
               end;
            end loop;
         end loop;
      end Bound_Cell;

      procedure Push (Centre, Radius : Dimensionless; Depth : Natural);
      --  Evaluate a new centre and queue its cell.

      procedure Push (Centre, Radius : Dimensionless; Depth : Natural) is
      begin
         Tail := Tail + 1;
         Queue (Tail).Centre := Centre;
         Queue (Tail).Radius := Radius;
         Queue (Tail).Depth := Depth;
         Evaluate_Jets (Centre, Queue (Tail).Jets);
      end Push;
   begin
      Sharp := False;
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         Sharp := True;
         return Result;
      elsif Length_Raw <= 0.0
        or else not Is_Finite (Length_Raw)
        or else not (Start_U >= 0.0 and then Start_U <= End_U and then End_U <= 1.0)
      then
         return Saturated;
      end if;

      for Axis in Axis_Name loop
         pragma Loop_Invariant (Active_Count <= Axis_Name'Pos (Axis));
         --  An algebraically constant axis without endpoint correction has no tangent at all.
         if not (Curve.Structurally_Constant_Axes (Axis)
                 and then
                   Curve.Evaluator_Data.Finish_Point (Axis) = Curve.Evaluator_Data.Uncorrected_Finish_Point (Axis))
         then
            Active_Count := Active_Count + 1;
            Active_Axes (Active_Count) := Axis;
         end if;
      end loop;
      if Active_Count = 0 then
         Sharp := True;
         return Result;
      end if;
      Gathered := Gather_Terms;
      if not Gathered then
         return Saturated;
      end if;

      if Term_Count = 0 then
         --  A pole-free evaluator has a constant rational tangent.
         for Index in 1 .. Active_Count loop
            Rational (Index, 0) := abs Constants (Index);
         end loop;
      else
         declare
            Middle  : constant Dimensionless := 0.5 * Start_U + 0.5 * End_U;
            Bounded : Boolean;
         begin
            --  A padded cell can reach past the range by its padding and by the rounding of its centre, which
            --  the padding exceeds.
            Evaluate_Sums
              (Middle,
               Dimensionless'Max (Middle - Start_U, End_U - Middle) * (1.0 + 2.0**(-50)) + 2.0 * Pad,
               Range_Sums,
               Bounded);
            if not Bounded then
               return Saturated;
            end if;
         end;

         declare
            Span  : constant Dimensionless := End_U - Start_U;
            Cells : constant Positive :=
              (if Span <= 8.0 * Dimensionless'Model_Epsilon * (1.0 + End_U)
               then 1
               elsif Span <= 0.125
               then 2
               elsif Span <= 0.5
               then 4
               else Base_Cells);
            Width : constant Dimensionless := Span / Dimensionless (Cells);
         begin
            --  Rounded centres and radii can leave gaps of a few units in the last place between neighbouring
            --  cells; Bound_Cell's padding of every radius closes them.
            for Index in 1 .. Cells loop
               Push
                 ((if Cells = 1 then 0.5 * Start_U + 0.5 * End_U else Start_U + (Dimensionless (Index) - 0.5) * Width),
                  0.5 * Width,
                  0);
            end loop;
         end;

         while Head <= Tail loop
            pragma Loop_Variant (Increases => Head);
            declare
               Item_Index : constant Positive := Head;
               Item       : Cell renames Queue (Item_Index);
               Radius     : constant Dimensionless := Item.Radius + Pad;
               Bounds     : Order_Bounds;
               Allowances : Order_Bounds;
               Bounded    : Boolean := True;
               Refine     : Boolean;
            begin
               --  Most cells are already resolved by the sums for the whole range, which need no pass over the
               --  poles. Only a cell which those leave loose is given its own, smaller sums.
               Bound_Cell (Item, Radius, Range_Sums, Bounds, Allowances, Refine);
               if Refine then
                  declare
                     Sums : Sum_Array;
                  begin
                     Evaluate_Sums (Item.Centre, Radius, Sums, Bounded);
                     if Bounded then
                        Bound_Cell (Item, Radius, Sums, Bounds, Allowances, Refine);
                     end if;
                  end;
               end if;
               if Refine
                 and then Item.Depth < Maximum_Depth
                 and then Evaluations + 2 <= Maximum_Evaluations
                 and then Tail + 3 <= Queue_Capacity
                 and then Item.Radius > 256.0 * Cell_Pad
               then
                  declare
                     Third : constant Dimensionless := Item.Radius / 3.0;
                  begin
                     Push (Item.Centre - 2.0 * Third, Third, Item.Depth + 1);
                     Push (Item.Centre + 2.0 * Third, Third, Item.Depth + 1);
                     Tail := Tail + 1;
                     Queue (Tail) :=
                       (Centre => Item.Centre, Radius => Third, Depth => Item.Depth + 1, Jets => Item.Jets);
                  end;
               else
                  --  Bounds are those of the range sums if the cell's own could not be formed.
                  for Index in 1 .. Active_Count loop
                     for Order in Majorant_Order loop
                        pragma Loop_Invariant (True);
                        if Bounds (Index, Order) > Rational (Index, Order) then
                           Rational (Index, Order) := Bounds (Index, Order);
                           Noise (Index, Order) := Allowances (Index, Order);
                        end if;
                        --  A limit stopped this cell while some bound was still loose. Whether that matters is only
                        --  known once Lower is final.
                        if Refine
                          and then Bounds (Index, Order) - 2.0 * Allowances (Index, Order) > Unresolved (Index, Order)
                        then
                           Unresolved (Index, Order) := Bounds (Index, Order) - 2.0 * Allowances (Index, Order);
                        end if;
                     end loop;
                  end loop;
               end if;
            end;
            Head := Head + 1;
         end loop;
      end if;

      for Index in 1 .. Active_Count loop
         declare
            Axis       : constant Axis_Name := Active_Axes (Index);
            Correction : constant Interval := Finish_Correction_Interval (Curve.Evaluator_Data, Axis);
            Ratio      : constant Interval :=
              (if Correction.Lower = 0.0 and then Correction.Upper = 0.0
               then Correction
               else Interval_Divide (Correction, Interval_Exact (Length_Raw)));
         begin
            if not Ratio.Valid then
               return Saturated;
            elsif not Correction_Bounds_Known and then not (Ratio.Lower = 0.0 and then Ratio.Upper = 0.0) then
               Correction_Bounds := [for Order in Majorant_Order => Correction_Bound (Order)];
               Correction_Bounds_Known := True;
            end if;
            for Order in Majorant_Order loop
               pragma Loop_Invariant (True);
               declare
                  Correction_Part : constant Dimensionless :=
                    (if Ratio.Lower = 0.0 and then Ratio.Upper = 0.0
                     then 0.0
                     else Up (Interval_Abs_Max (Ratio) * Correction_Bounds (Order)));
                  Combined        : Dimensionless :=
                    (if Correction_Part = 0.0
                     then Rational (Index, Order)
                     else Up (Rational (Index, Order) + Correction_Part));
               begin
                  if not Is_Finite (Combined) or else Combined >= Dimensionless'Last then
                     return Saturated;
                  end if;
                  if Order = 0 then
                     --  The ideal tangent is a unit vector S in frame coordinates, so its component on this axis is
                     --  at most the Euclidean norm of the frame's coefficients for the axis. The retained tangent
                     --  is within the certified tangent error of the ideal one. This is exactly one for an
                     --  orthonormal frame and does not depend on how finely the range was resolved.
                     declare
                        Square_Sum : Dimensionless := 0.0;
                        Cap        : Dimensionless;
                     begin
                        for Component in Frame_Component_Index loop
                           pragma Loop_Invariant (True);
                           Square_Sum :=
                             Up (Square_Sum + Up (Curve.Frame (Component) (Axis) * Curve.Frame (Component) (Axis)));
                        end loop;
                        Cap := Certified_Upper_Square_Root (Square_Sum);
                        if Cap < Dimensionless'Last then
                           Cap := Up (Cap + Up (Curve.Certified_Tangent_Error + Correction_Part));
                           Combined := Dimensionless'Min (Combined, Cap);
                        end if;
                     end;
                  end if;
                  Result (Axis) (Order) := Combined;
               end;
            end loop;
         end;
      end loop;
      Sharp :=
        (for all Index in 1 .. Active_Count =>
           (for all Order in Majorant_Order =>
              Noise (Index, Order) <= Noise_Fraction * Rational (Index, Order) + Noise_Floor
              and then Unresolved (Index, Order) <= (1.0 + Loose_Fraction) * Lower (Index, Order) + Noise_Floor));
      return Result;
   exception
      when Constraint_Error =>
         Sharp := False;
         return Saturated;
   end Pole_Taylor_Tangent_Range_Majorants;

   function Bernstein_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless; Success : out Boolean) return Axis_Majorants
   is
      --  This path operates on the common-denominator Bernstein certificate which construction or
      --  Retain_Bernstein_Tangent_Certificate stores in Curve. It reconstructs each factorial-scaled derivative as a
      --  rational Bernstein polynomial, restricts it to the requested range, and bounds it by outward-rounded De
      --  Casteljau subdivision. The smooth endpoint correction is bounded independently and added only after the
      --  rational part has been certified.
      subtype Polynomial_Index is Natural range 0 .. Maximum_Derivative_Bernstein_Degree;
      subtype Polynomial is Interval_Polynomial (Polynomial_Index);
      type Axis_Polynomials is array (Axis_Name) of Polynomial;
      type Axis_Bounds is array (Axis_Name) of Dimensionless;
      type Axis_Flags is array (Axis_Name) of Boolean;
      type Correction_Bound_Array is array (Majorant_Order) of Dimensionless;

      Maximum_Subdivision_Depth : constant := 2;
      --  Four leaf hulls retain margin below the five-percent dense-oracle tolerance, including the supported 20:1
      --  warps, while keeping the subdivision work small and fixed.

      Tightness_Factor : constant Dimensionless := 1.02;
      --  Accept a valid rational hull once it is within two percent of the magnitude already forced by a segment
      --  endpoint. Axes which remain looser are subdivided independently.

      Tightness_Floor : constant Dimensionless := 1.0E-12;
      --  Absolute allowance in the tightness comparison. Without this floor, an axis whose true value is near zero
      --  would always consume the full subdivision depth despite already having a negligible bound.

      Certificate              : Retained_Tangent_Bernstein_Certificate renames Curve.Retained_Tangent_Certificate;
      Q_Degree                 : constant Natural := Certificate.Degree;
      Q                        : Polynomial := [others => Interval_Exact (0.0)];
      Q_Derivative             : Polynomial := [others => Interval_Exact (0.0)];
      Q_Derivative_Degree      : Natural := 0;
      Denominator_Power        : Polynomial := [others => Interval_Exact (0.0)];
      Denominator_Power_Degree : Natural := Q_Degree;
      Numerators               : Axis_Polynomials := [others => [others => Interval_Exact (0.0)]];
      Numerator_Degree         : Natural range 0 .. Maximum_Derivative_Bernstein_Degree := Q_Degree;
      Zero_Numerator_Axes      : Axis_Flags := [others => False];
      Result                   : Axis_Majorants := [others => [others => 0.0]];
      Length_Raw               : constant Dimensionless := Dimensionless (Curve.Evaluator_Data.Length_Value / mm);

      --  Return the package's fail-closed interval sentinel. Propagating this value makes any unsafe intermediate
      --  calculation turn into Success = False instead of an apparently finite certificate.
      function Invalid_Interval return Interval
      is (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);

      function Multiply_Bernstein
        (Left : Polynomial; Left_Degree : Natural; Right : Polynomial; Right_Degree : Natural) return Polynomial;
      --  Multiply active controls and pad the shared kernel's result to the derivative workspace size.

      function Multiply_Bernstein
        (Left : Polynomial; Left_Degree : Natural; Right : Polynomial; Right_Degree : Natural) return Polynomial
      is
         Result : Polynomial := [others => Interval_Exact (0.0)];
      begin
         Result (0 .. Left_Degree + Right_Degree) :=
           Multiply_Bernstein (Left (0 .. Left_Degree), Right (0 .. Right_Degree));
         return Result;
      exception
         when Constraint_Error =>
            return [others => Invalid_Interval];
      end Multiply_Bernstein;

      --  Differentiate a Bernstein polynomial by taking scaled forward differences of adjacent controls.
      function Differentiate_Bernstein
        (Source : Polynomial; Source_Degree : Natural; Result_Degree : out Natural) return Polynomial
      with Side_Effects;
      --  Differentiate a Bernstein polynomial and return its resulting degree.

      function Differentiate_Bernstein
        (Source : Polynomial; Source_Degree : Natural; Result_Degree : out Natural) return Polynomial
      is
         Result : Polynomial := [others => Interval_Exact (0.0)];
      begin
         if Source_Degree = 0 then
            Result_Degree := 0;
            return Result;
         end if;
         Result_Degree := Source_Degree - 1;
         for Index in 0 .. Result_Degree loop
            Result (Index) :=
              Interval_Multiply
                (Interval_Exact (Dimensionless (Source_Degree)),
                 Interval_Subtract (Source (Index + 1), Source (Index)));
         end loop;
         return Result;
      exception
         when Constraint_Error =>
            Result_Degree := 0;
            return [others => Invalid_Interval];
      end Differentiate_Bernstein;

      --  Multiply every active control by the same enclosing scale.

      --  Recognize an algebraically zero interval polynomial. This is deliberately exact: a merely small numerator
      --  still needs normal certification and must not be discarded as numerical noise.
      function Is_Exact_Zero (Value : Polynomial; Degree : Natural) return Boolean;
      --  Test whether every active coefficient is the exact zero interval.

      function Is_Exact_Zero (Value : Polynomial; Degree : Natural) return Boolean is
      begin
         return
           (for all Index in 0 .. Degree =>
              Value (Index).Valid and then Value (Index).Lower = 0.0 and then Value (Index).Upper = 0.0);
      end Is_Exact_Zero;

      --  Split Source at an interval-enclosed parameter using the De Casteljau triangle. Left and Right represent the
      --  two restricted polynomial pieces, including uncertainty in the split parameter.
      procedure Split_Bernstein (Source : Polynomial; Degree : Natural; T : Interval; Left, Right : out Polynomial);
      --  Split a Bernstein polynomial at the interval parameter T.

      procedure Split_Bernstein (Source : Polynomial; Degree : Natural; T : Interval; Left, Right : out Polynomial) is
         Work        : Polynomial := Source;
         One_Minus_T : constant Interval := Interval_Subtract (Interval_Exact (1.0), T);

         function Half (Value : Interval) return Interval;
         --  Scale an interval by one half with outward rounding.

         function Half (Value : Interval) return Interval is
         begin
            if not Value.Valid then
               return Invalid_Interval;
            end if;
            return Checked_Interval (Down (0.5 * Value.Lower), Up (0.5 * Value.Upper));
         end Half;

         function Interpolate (First, Last : Interval) return Interval;
         --  Interpolate two coefficients using the enclosing split parameter.

         function Interpolate (First, Last : Interval) return Interval is
         begin
            if T.Valid and then T.Lower = 0.5 and then T.Upper = 0.5 then
               --  Keep invalid-control midpoint propagation cheap on the generic path without using the four-product
               --  interval multiplier. Certified adaptive refinements use the dedicated splitter below.
               return Interval_Add (Half (First), Half (Last));
            else
               return Interval_Add (Interval_Multiply (One_Minus_T, First), Interval_Multiply (T, Last));
            end if;
         end Interpolate;
      begin
         Left := [others => Interval_Exact (0.0)];
         Right := [others => Interval_Exact (0.0)];
         Left (0) := Work (0);
         Right (Degree) := Work (Degree);
         if Degree > 0 then
            for Level in 1 .. Degree loop
               for Index in 0 .. Degree - Level loop
                  Work (Index) := Interpolate (Work (Index), Work (Index + 1));
               end loop;
               Left (Level) := Work (0);
               Right (Degree - Level) := Work (Degree - Level);
            end loop;
         end if;
      exception
         when Constraint_Error =>
            Left := [others => Invalid_Interval];
            Right := [others => Invalid_Interval];
      end Split_Bernstein;

      --  Specialized De Casteljau split at one half for a single polynomial. It avoids repeated generic interval
      --  multiplication on the adaptive-certification path while retaining the generic fallback for invalid inputs.
      procedure Split_Bernstein_Midpoint (Source : Polynomial; Degree : Natural; Left, Right : out Polynomial);
      --  Split a Bernstein polynomial at the exact midpoint.

      procedure Split_Bernstein_Midpoint (Source : Polynomial; Degree : Natural; Left, Right : out Polynomial) is
         pragma Suppress (Overflow_Check);
         type Endpoint_Workspace is array (Polynomial'Range) of Dimensionless;
         Lower, Upper : Endpoint_Workspace := [others => 0.0];
         --  The preflight bounds the entire midpoint triangle and validates its workspace indices;
         --  extreme controls go through Split_Bernstein, whose arithmetic retains overflow checks.
      begin
         if Degree not in Polynomial'Range then
            Left := [others => Invalid_Interval];
            Right := [others => Invalid_Interval];
            return;
         end if;
         --  A convex midpoint cannot increase the endpoint magnitude before rounding. Starting below Last/4 leaves
         --  ample room for the outward inflation at all 84 levels, including Model_Small padding. This establishes
         --  finiteness for the entire triangle and avoids rechecking it at every node. Extreme or invalid controls
         --  retain the generic splitter's checked arithmetic.
         for Index in 0 .. Degree loop
            if not Source (Index).Valid
              or else not (abs Source (Index).Lower <= Dimensionless'Last / 4.0)
              or else not (abs Source (Index).Upper <= Dimensionless'Last / 4.0)
            then
               Split_Bernstein (Source, Degree, Interval_Exact (0.5), Left, Right);
               return;
            end if;
         end loop;

         declare
            pragma Suppress (Index_Check);
            pragma Suppress (Range_Check);
            --  The explicit degree guard proves that Source(0..Degree) is in the fixed workspace range.
            --  Each triangle row has Index+1 <= Degree and 1 <= Level <= Degree.
         begin
            Left := [others => Interval_Exact (0.0)];
            Right := [others => Interval_Exact (0.0)];
            for Index in 0 .. Degree loop
               Lower (Index) := Source (Index).Lower;
               Upper (Index) := Source (Index).Upper;
            end loop;
            Left (0) := Source (0);
            Right (Degree) := Source (Degree);
            for Level in 1 .. Degree loop
               --  Separate endpoint arrays keep validity flags out of the quadratic workspace.
               --  Every active source control is already valid.
               for Index in 0 .. Degree - Level loop
                  if Lower (Index) /= Lower (Index + 1) or else Upper (Index) /= Upper (Index + 1) then
                     Lower (Index) := 0.5 * Lower (Index) + 0.5 * Lower (Index + 1);
                     Upper (Index) := 0.5 * Upper (Index) + 0.5 * Upper (Index + 1);
                     Inflate_Finite_Endpoints (Lower (Index), Upper (Index));
                  end if;
               end loop;
               Left (Level) := (Lower => Lower (0), Upper => Upper (0), Valid => True);
               Right (Degree - Level) :=
                 (Lower => Lower (Degree - Level), Upper => Upper (Degree - Level), Valid => True);
            end loop;
         end;
      exception
         when Constraint_Error =>
            Left := [others => Invalid_Interval];
            Right := [others => Invalid_Interval];
      end Split_Bernstein_Midpoint;

      --  Evaluate Source at T by splitting it and taking the shared boundary control.
      function Evaluate_Bernstein (Source : Polynomial; Degree : Natural; T : Interval) return Interval;
      --  Evaluate a Bernstein polynomial over an interval parameter using de Casteljau subdivision.

      function Evaluate_Bernstein (Source : Polynomial; Degree : Natural; T : Interval) return Interval is
         Left, Right : Polynomial;
      begin
         Split_Bernstein (Source, Degree, T, Left, Right);
         return Left (Degree);
      end Evaluate_Bernstein;

      --  Reparameterize Source onto the requested closed subrange. A point-sized range becomes a degree-zero value;
      --  otherwise successive left and right splits return controls whose domain is again zero through one.
      function Restrict_Bernstein
        (Source : Polynomial; Degree : Natural; Range_Start, Range_End : Dimensionless) return Polynomial;
      --  Restrict a Bernstein polynomial to the requested parameter subrange.

      function Restrict_Bernstein
        (Source : Polynomial; Degree : Natural; Range_Start, Range_End : Dimensionless) return Polynomial
      is
         Work        : Polynomial := Source;
         Left, Right : Polynomial;
      begin
         if Range_Start = Range_End then
            Work := [others => Interval_Exact (0.0)];
            Work (0) := Evaluate_Bernstein (Source, Degree, Interval_Exact (Range_Start));
            return Work;
         end if;
         if Range_Start > 0.0 then
            Split_Bernstein (Work, Degree, Interval_Exact (Range_Start), Left, Right);
            Work := Right;
         end if;
         if Range_End < 1.0 then
            declare
               Local_End : constant Interval :=
                 Interval_Divide
                   (Interval_Subtract (Interval_Exact (Range_End), Interval_Exact (Range_Start)),
                    Interval_Subtract (Interval_Exact (1.0), Interval_Exact (Range_Start)));
            begin
               Split_Bernstein (Work, Degree, Local_End, Left, Right);
               Work := Left;
            end;
         end if;
         return Work;
      exception
         when Constraint_Error =>
            return [others => Invalid_Interval];
      end Restrict_Bernstein;

      --  Return a lower bound on absolute value. This is used only to decide whether subdivision could materially
      --  tighten a valid upper bound; an interval spanning zero provides no positive evidence.
      function Interval_Abs_Min (Value : Interval) return Dimensionless;
      --  Return the minimum absolute magnitude represented by an interval.

      function Interval_Abs_Min (Value : Interval) return Dimensionless is
      begin
         if not Value.Valid or else (Value.Lower <= 0.0 and then Value.Upper >= 0.0) then
            return 0.0;
         elsif Value.Lower > 0.0 then
            return Value.Lower;
         else
            return -Value.Upper;
         end if;
      end Interval_Abs_Min;

      --  Bound all active rational functions N_axis / Q on the requested chart-coordinate range. Numerators and the
      --  positive denominator are elevated to a common degree, restricted together, and adaptively subdivided until
      --  the rational Bernstein hull is acceptably close to values already forced at the subrange endpoints.
      function Rational_Range_Bounds
        (Axis_Numerators        : Axis_Polynomials;
         Numerator_Degree       : Natural;
         Denominator            : Polynomial;
         Denominator_Degree     : Natural;
         Active_Axes            : Axis_Flags;
         Range_Start, Range_End : Dimensionless;
         Fallback_Denominator   : Dimensionless) return Axis_Bounds;
      --  Bound active rational Bernstein functions over a parameter subrange.

      function Rational_Range_Bounds
        (Axis_Numerators        : Axis_Polynomials;
         Numerator_Degree       : Natural;
         Denominator            : Polynomial;
         Denominator_Degree     : Natural;
         Active_Axes            : Axis_Flags;
         Range_Start, Range_End : Dimensionless;
         Fallback_Denominator   : Dimensionless) return Axis_Bounds
      is
         Degree                 : constant Natural := Natural'Max (Numerator_Degree, Denominator_Degree);
         Restricted_Numerators  : Axis_Polynomials := [others => [others => Interval_Exact (0.0)]];
         Restricted_Denominator : constant Polynomial :=
           Restrict_Bernstein
             (Elevate_Bernstein (Denominator, Denominator_Degree, Degree), Degree, Range_Start, Range_End);

         --  Compute the convex-hull bound for one rational Bernstein segment. When every denominator control is
         --  positive, N_i / Q_i are controls of the same rational function under positive normalized weights.
         procedure Hull_Bounds
           (Local_Numerators  : Axis_Polynomials;
            Local_Denominator : Polynomial;
            Local_Active      : Axis_Flags;
            Bounds            : out Axis_Bounds;
            Valid             : out Axis_Flags);

         --  Obtain conservative lower bounds on the magnitudes at the two segment endpoints. These are not returned as
         --  certificates; they only provide a scale for deciding whether the current upper hull deserves refinement.
         function Point_Lower_Bounds
           (Local_Numerators : Axis_Polynomials; Local_Denominator : Polynomial; Local_Active : Axis_Flags)
            return Axis_Bounds;

         --  Recursively split loose axes while accepting already tight axes at the current node.
         function Refined_Bounds
           (Local_Numerators  : Axis_Polynomials;
            Local_Denominator : Polynomial;
            Local_Active      : Axis_Flags;
            Depth             : Natural) return Axis_Bounds
         with Subprogram_Variant => (Increases => Depth);

         procedure Hull_Bounds
           (Local_Numerators  : Axis_Polynomials;
            Local_Denominator : Polynomial;
            Local_Active      : Axis_Flags;
            Bounds            : out Axis_Bounds;
            Valid             : out Axis_Flags)
         is
            Denominator_Is_Valid : Boolean := True;
            Reciprocal_Uppers    : array (Polynomial_Index) of Dimensionless := [others => 0.0];

            function Control_Bound (Numerator : Interval; Reciprocal_Upper : Dimensionless) return Dimensionless;
            --  Only the absolute upper bound is needed for a hull; a full signed quotient interval is unnecessary.

            function Control_Bound (Numerator : Interval; Reciprocal_Upper : Dimensionless) return Dimensionless is
               Magnitude : constant Dimensionless := Interval_Abs_Max (Numerator);
               Bound     : Dimensionless;
            begin
               if not Numerator.Valid then
                  return Dimensionless'Last;
               elsif Magnitude = 0.0 then
                  return 0.0;
               end if;
               Bound := Up (Magnitude * Reciprocal_Upper);
               return (if Is_Finite (Bound) then Bound else Dimensionless'Last);
            exception
               when Constraint_Error =>
                  return Dimensionless'Last;
            end Control_Bound;
         begin
            Bounds := [others => 0.0];
            Valid := [others => False];
            for Index in 0 .. Degree loop
               if not Local_Denominator (Index).Valid or else Local_Denominator (Index).Lower <= 0.0 then
                  Denominator_Is_Valid := False;
                  exit;
               end if;
               declare
                  Reciprocal : constant Interval := Interval_Divide (Interval_Exact (1.0), Local_Denominator (Index));
               begin
                  if not Reciprocal.Valid then
                     Denominator_Is_Valid := False;
                     exit;
                  end if;
                  Reciprocal_Uppers (Index) := Reciprocal.Upper;
               end;
            end loop;
            if not Denominator_Is_Valid then
               for Axis in Axis_Name loop
                  if Local_Active (Axis) then
                     Bounds (Axis) := Dimensionless'Last;
                  end if;
               end loop;
               return;
            end if;
            for Axis in Axis_Name loop
               if Local_Active (Axis) then
                  Valid (Axis) := True;
                  for Index in 0 .. Degree loop
                     declare
                        Bound : constant Dimensionless :=
                          Control_Bound (Local_Numerators (Axis) (Index), Reciprocal_Uppers (Index));
                     begin
                        if Bound >= Dimensionless'Last then
                           Bounds (Axis) := Dimensionless'Last;
                           Valid (Axis) := False;
                           exit;
                        end if;
                        Bounds (Axis) := Dimensionless'Max (Bounds (Axis), Bound);
                     end;
                  end loop;
                  if Valid (Axis) then
                     Bounds (Axis) := Up (Bounds (Axis));
                  end if;
               end if;
            end loop;
         end Hull_Bounds;

         function Point_Lower_Bounds
           (Local_Numerators : Axis_Polynomials; Local_Denominator : Polynomial; Local_Active : Axis_Flags)
            return Axis_Bounds
         is
            Result           : Axis_Bounds := [others => 0.0];
            First_Reciprocal : constant Interval := Interval_Divide (Interval_Exact (1.0), Local_Denominator (0));
            Last_Reciprocal  : constant Interval := Interval_Divide (Interval_Exact (1.0), Local_Denominator (Degree));
         begin
            for Axis in Axis_Name loop
               if Local_Active (Axis) then
                  declare
                     First : constant Interval := Interval_Multiply (Local_Numerators (Axis) (0), First_Reciprocal);
                     Last  : constant Interval :=
                       Interval_Multiply (Local_Numerators (Axis) (Degree), Last_Reciprocal);
                  begin
                     Result (Axis) := Dimensionless'Max (Interval_Abs_Min (First), Interval_Abs_Min (Last));
                  end;
               end if;
            end loop;
            return Result;
         end Point_Lower_Bounds;

         --  Fall back to the numerator control hull divided by the independently certified global denominator lower
         --  bound. This is less tight than paired rational controls but remains valid if local denominator controls do
         --  not individually prove positivity.
         function Fallback_Bound (Local_Numerator : Polynomial) return Dimensionless;
         --  Produce a conservative numerator bound when subdivision cannot certify the ratio.

         function Fallback_Bound (Local_Numerator : Polynomial) return Dimensionless is
            Numerator_Bound : Dimensionless := 0.0;
         begin
            if Fallback_Denominator <= 0.0 then
               return Dimensionless'Last;
            end if;
            for Index in 0 .. Degree loop
               if not Local_Numerator (Index).Valid then
                  return Dimensionless'Last;
               end if;
               Numerator_Bound := Dimensionless'Max (Numerator_Bound, Interval_Abs_Max (Local_Numerator (Index)));
            end loop;
            declare
               Quotient : constant Dimensionless := Up (Numerator_Bound / Fallback_Denominator);
            begin
               return
                 (if Is_Finite (Quotient) and then Quotient < Dimensionless'Last
                  then Quotient
                  else Dimensionless'Last);
            end;
         end Fallback_Bound;

         function Refined_Bounds
           (Local_Numerators  : Axis_Polynomials;
            Local_Denominator : Polynomial;
            Local_Active      : Axis_Flags;
            Depth             : Natural) return Axis_Bounds
         is
            Current    : Axis_Bounds;
            Hull_Valid : Axis_Flags;
            Lower      : Axis_Bounds := [others => 0.0];
            Need_Split : Axis_Flags := [others => False];
            Result     : Axis_Bounds := [others => 0.0];
            Any_Split  : Boolean := False;
         begin
            Hull_Bounds (Local_Numerators, Local_Denominator, Local_Active, Current, Hull_Valid);
            if Depth < Maximum_Subdivision_Depth then
               Lower := Point_Lower_Bounds (Local_Numerators, Local_Denominator, Local_Active);
            end if;
            for Axis in Axis_Name loop
               if Local_Active (Axis) then
                  if Hull_Valid (Axis)
                    and then
                      (Depth = Maximum_Subdivision_Depth
                       or else Current (Axis) <= Tightness_Factor * Lower (Axis) + Tightness_Floor)
                  then
                     Result (Axis) := Current (Axis);
                  elsif Depth = Maximum_Subdivision_Depth then
                     Result (Axis) := Fallback_Bound (Local_Numerators (Axis));
                  else
                     Need_Split (Axis) := True;
                     Any_Split := True;
                  end if;
               end if;
            end loop;
            if Any_Split then
               declare
                  Left_Numerators, Right_Numerators   : Axis_Polynomials;
                  Left_Denominator, Right_Denominator : Polynomial;
                  Left_Bounds, Right_Bounds           : Axis_Bounds;
               begin
                  --  Split the denominator once and use the same polynomial splitter for each loose axis.
                  Split_Bernstein_Midpoint (Local_Denominator, Degree, Left_Denominator, Right_Denominator);
                  Left_Numerators := [others => [others => Interval_Exact (0.0)]];
                  Right_Numerators := [others => [others => Interval_Exact (0.0)]];
                  for Axis in Axis_Name loop
                     if Need_Split (Axis) then
                        Split_Bernstein_Midpoint
                          (Local_Numerators (Axis), Degree, Left_Numerators (Axis), Right_Numerators (Axis));
                     end if;
                  end loop;
                  Left_Bounds := Refined_Bounds (Left_Numerators, Left_Denominator, Need_Split, Depth + 1);
                  Right_Bounds := Refined_Bounds (Right_Numerators, Right_Denominator, Need_Split, Depth + 1);
                  for Axis in Axis_Name loop
                     if Need_Split (Axis) then
                        Result (Axis) := Dimensionless'Max (Left_Bounds (Axis), Right_Bounds (Axis));
                     end if;
                  end loop;
               end;
            end if;
            return Result;
         end Refined_Bounds;
      begin
         for Axis in Axis_Name loop
            if Active_Axes (Axis) then
               Restricted_Numerators (Axis) :=
                 Restrict_Bernstein
                   (Elevate_Bernstein (Axis_Numerators (Axis), Numerator_Degree, Degree),
                    Degree,
                    Range_Start,
                    Range_End);
            end if;
         end loop;
         if Range_Start = Range_End then
            declare
               Result     : Axis_Bounds := [others => 0.0];
               Reciprocal : constant Interval := Interval_Divide (Interval_Exact (1.0), Restricted_Denominator (0));
            begin
               for Axis in Axis_Name loop
                  if Active_Axes (Axis) then
                     Result (Axis) :=
                       Interval_Abs_Max (Interval_Multiply (Restricted_Numerators (Axis) (0), Reciprocal));
                  end if;
               end loop;
               return Result;
            end;
         else
            return Refined_Bounds (Restricted_Numerators, Restricted_Denominator, Active_Axes, 0);
         end if;
      exception
         when Constraint_Error =>
            return [others => Dimensionless'Last];
      end Rational_Range_Bounds;

      --  Bound an ordinary Bernstein polynomial by recursively taking absolute control hulls. The endpoint-correction
      --  polynomial is small, so a fixed depth gives a tight inexpensive bound without rational machinery.
      function Polynomial_Subdivision_Bound (Value : Polynomial; Degree, Depth : Natural) return Dimensionless
      with Subprogram_Variant => (Decreases => Depth);
      --  Bound a polynomial by recursively subdividing its Bernstein control polygon.

      function Polynomial_Subdivision_Bound (Value : Polynomial; Degree, Depth : Natural) return Dimensionless is
      begin
         if Depth = 0 or else Degree = 0 then
            declare
               Bound : Dimensionless := 0.0;
            begin
               for Index in 0 .. Degree loop
                  Bound := Dimensionless'Max (Bound, Interval_Abs_Max (Value (Index)));
               end loop;
               return Up (Bound);
            end;
         end if;
         declare
            Left, Right : Polynomial;
         begin
            Split_Bernstein_Midpoint (Value, Degree, Left, Right);
            return
              Dimensionless'Max
                (Polynomial_Subdivision_Bound (Left, Degree, Depth - 1),
                 Polynomial_Subdivision_Bound (Right, Degree, Depth - 1));
         end;
      end Polynomial_Subdivision_Bound;

      --  Bound the selected factorial-scaled derivative of the degree-eleven endpoint smootherstep on the requested
      --  physical-parameter range. The correction displacement divided by curve length is applied later per axis.
      function Correction_Coefficient_Bound (Order : Majorant_Order) return Dimensionless;
      --  Return the cached or computed bound for one correction order.

      function Correction_Coefficient_Bound (Order : Majorant_Order) return Dimensionless is
         Work   : Polynomial := [others => Interval_Exact (0.0)];
         Degree : constant Natural := 10 - Order;
      begin
         if Start_U = 0.0 and then End_U = 1.0 then
            return Full_Correction_Bounds (Order);
         end if;
         Work (0 .. Degree) := Correction_Derivatives (Order) (0 .. Degree);
         Work := Restrict_Bernstein (Work, Degree, Start_U, End_U);
         if Start_U = End_U then
            return Interval_Abs_Max (Work (0));
         else
            return Polynomial_Subdivision_Bound (Work, Degree, 4);
         end if;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Correction_Coefficient_Bound;

      --  Map normalized physical distance U to polynomial chart coordinate V with an outward-rounded enclosure of the
      --  endpoint-preserving Möbius warp. Exact endpoints remain exact to avoid widening full-range queries.
      function Warp_Interval (U : Dimensionless) return Interval;
      --  Enclose the distance-warp mapping at U.

      function Warp_Interval (U : Dimensionless) return Interval is
         U_Interval  : constant Interval := Interval_Exact (U);
         W_Interval  : constant Interval := Interval_Exact (Curve.Warp_Factor);
         Numerator   : Interval;
         Denominator : Interval;
      begin
         if U = 0.0 then
            return Interval_Exact (0.0);
         elsif U = 1.0 then
            return Interval_Exact (1.0);
         end if;
         Numerator := Interval_Multiply (W_Interval, U_Interval);
         Denominator := Interval_Add (Interval_Subtract (Interval_Exact (1.0), U_Interval), Numerator);
         return Interval_Divide (Numerator, Denominator);
      exception
         when Constraint_Error =>
            return Invalid_Interval;
      end Warp_Interval;

      Start_V           : Dimensionless;
      End_V             : Dimensionless;
      G                 : Polynomial := [others => Interval_Exact (0.0)];
      G_Degree          : Natural range 0 .. 2;
      Correction_Bounds : Correction_Bound_Array := [others => 0.0];

      --  Advance from A_Order = N / Q**(Order + 1) to the next factorial-scaled physical-distance derivative. In chart
      --  coordinate V the recurrence is
      --
      --     N_next = (dV/dU) * (N' * Q / (Order + 1) - N * Q'),
      --
      --  while the denominator gains one additional factor of Q. G stores dV/dU as a degree-two polynomial in V.
      procedure Advance_Numerators (Order : Majorant_Order);
      --  Advance the rational derivative numerator recurrence to Order.

      procedure Advance_Numerators (Order : Majorant_Order) is
         Derivative_Degree  : Natural;
         Difference_Degree  : Natural;
         Next_Degree        : Natural;
         Divisor            : constant Dimensionless := Dimensionless (Natural (Order) + 1);
         Reciprocal_Divisor : constant Interval := Checked_Interval (Down (1.0 / Divisor), Up (1.0 / Divisor));
      begin
         if Q_Degree = 0 then
            --  A constant rational tangent has no higher derivatives; endpoint correction is added separately.
            Numerators := [others => [others => Interval_Exact (0.0)]];
            Zero_Numerator_Axes := [others => True];
            Numerator_Degree := 0;
            return;
         end if;

         Difference_Degree := Numerator_Degree - 1 + Q_Degree;
         Next_Degree := Difference_Degree + G_Degree;
         for Axis in Axis_Name loop
            if not Zero_Numerator_Axes (Axis) then
               declare
                  Derivative     : Polynomial;
                  First_Product  : Polynomial;
                  Second_Product : Polynomial;
                  Difference     : Polynomial := [others => Interval_Exact (0.0)];
               begin
                  Derivative := Differentiate_Bernstein (Numerators (Axis), Numerator_Degree, Derivative_Degree);
                  First_Product := Multiply_Bernstein (Derivative, Derivative_Degree, Q, Q_Degree);
                  Second_Product :=
                    Multiply_Bernstein (Numerators (Axis), Numerator_Degree, Q_Derivative, Q_Derivative_Degree);
                  for Index in 0 .. Difference_Degree loop
                     Difference (Index) :=
                       Interval_Subtract
                         (Interval_Multiply (Reciprocal_Divisor, First_Product (Index)), Second_Product (Index));
                  end loop;
                  Numerators (Axis) :=
                    (if G_Degree = 0
                     then Difference
                     else Multiply_Bernstein (G, G_Degree, Difference, Difference_Degree));
                  Zero_Numerator_Axes (Axis) := Is_Exact_Zero (Numerators (Axis), Next_Degree);
               end;
            end if;
         end loop;
         Numerator_Degree := Next_Degree;
      end Advance_Numerators;
   begin
      Success := False;
      --  Handle representations whose exact structure already supplies the answer before consulting the retained
      --  certificate. A zero curve has zero majorants, while a pole-free evaluator without endpoint correction has a
      --  constant tangent and therefore only order-zero values.
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         Success := True;
         return Result;
      elsif Curve.Evaluator_Data.Antiderivative_Cache.Real_Pole_Count = 0
        and then Curve.Evaluator_Data.Antiderivative_Cache.Pair_Count = 0
        and then
          (for all Axis in Axis_Name =>
             Curve.Evaluator_Data.Finish_Point (Axis) = Curve.Evaluator_Data.Uncorrected_Finish_Point (Axis))
      then
         --  With no poles and no endpoint correction, the installed evaluator itself has a constant tangent. Besides
         --  making the exact straight-line result explicit, this avoids manufacturing roundoff-sized higher
         --  derivatives by differentiating interval controls whose numerator and denominator are proportional.
         for Axis in Axis_Name loop
            Result (Axis) (0) := abs Curve.Evaluator_Data.Antiderivative_Cache.Constant_Tangent (Axis);
         end loop;
         Success := True;
         return Result;
      elsif not Certificate.Valid
        or else Certificate.Minimum_Denominator <= 0.0
        or else not Is_Finite (Certificate.Minimum_Denominator)
        or else Length_Raw <= 0.0
        or else not Is_Finite (Length_Raw)
      then
         return Result;
      end if;

      declare
         Start_Image : constant Interval := Warp_Interval (Start_U);
         End_Image   : constant Interval := Warp_Interval (End_U);
      begin
         if not Start_Image.Valid or else not End_Image.Valid then
            return Result;
         end if;
         --  The positive-W Möbius warp is monotone. Using the lower enclosure of its start image and the upper
         --  enclosure of its end image retains every point in the requested U range despite endpoint roundoff.
         Start_V := Dimensionless'Max (0.0, Start_Image.Lower);
         End_V := Dimensionless'Min (1.0, End_Image.Upper);
         if Start_V > End_V then
            return Result;
         end if;
      end;

      --  Copy the active retained controls into degree-eighty-four workspaces. Numerators initially represent the
      --  tangent itself, and Denominator_Power initially contains Q; each derivative step updates both together.
      for Index in 0 .. Q_Degree loop
         Q (Index) := Certificate.Denominator (Index);
         Denominator_Power (Index) := Q (Index);
         for Axis in Axis_Name loop
            Numerators (Axis) (Index) := Certificate.Axis_Numerators (Axis) (Index);
         end loop;
      end loop;
      for Axis in Axis_Name loop
         Zero_Numerator_Axes (Axis) := Is_Exact_Zero (Numerators (Axis), Numerator_Degree);
      end loop;
      Q_Derivative := Differentiate_Bernstein (Q, Q_Degree, Q_Derivative_Degree);

      --  Express dV/dU in Bernstein form as a polynomial in V. The identity warp has the exact constant value one;
      --  otherwise the Möbius derivative has quadratic controls W, 1, and 1/W.
      if Curve.Warp_Factor = 1.0 then
         G_Degree := 0;
         G (0) := Interval_Exact (1.0);
      else
         G_Degree := 2;
         G (0) := Interval_Exact (Curve.Warp_Factor);
         G (1) := Interval_Exact (1.0);
         G (2) := Interval_Divide (Interval_Exact (1.0), Interval_Exact (Curve.Warp_Factor));
      end if;

      for Order in Majorant_Order loop
         Correction_Bounds (Order) := Correction_Coefficient_Bound (Order);
         if Correction_Bounds (Order) >= Dimensionless'Last or else not Is_Finite (Correction_Bounds (Order)) then
            return Result;
         end if;
      end loop;

      --  Certify each derivative order before advancing the rational recurrence. Structural zero axes bypass interval
      --  work, and the endpoint smootherstep contribution is combined only after the rational part has a finite bound.
      for Order in Majorant_Order loop
         declare
            Fallback_Denominator : constant Dimensionless := Lower_Power (Certificate.Minimum_Denominator, Order + 1);
            Active_Rational_Axes : constant Axis_Flags :=
              [for Axis in Axis_Name =>
                 not Zero_Numerator_Axes (Axis)
                 and then
                   not (Curve.Structurally_Constant_Axes (Axis)
                        and then
                          Curve.Evaluator_Data.Finish_Point (Axis)
                          = Curve.Evaluator_Data.Uncorrected_Finish_Point (Axis))];
            Any_Active           : constant Boolean := (for some Axis in Axis_Name => Active_Rational_Axes (Axis));
            Rational_Bounds      : constant Axis_Bounds :=
              (if Any_Active
               then
                 Rational_Range_Bounds
                   (Numerators,
                    Numerator_Degree,
                    Denominator_Power,
                    Denominator_Power_Degree,
                    Active_Rational_Axes,
                    Start_V,
                    End_V,
                    Fallback_Denominator)
               else [others => 0.0]);
         begin
            for Axis in Axis_Name loop
               if Curve.Structurally_Constant_Axes (Axis)
                 and then
                   Curve.Evaluator_Data.Finish_Point (Axis) = Curve.Evaluator_Data.Uncorrected_Finish_Point (Axis)
               then
                  Result (Axis) (Order) := 0.0;
               else
                  declare
                     Rational_Bound   : constant Dimensionless := Rational_Bounds (Axis);
                     Correction       : constant Interval := Finish_Correction_Interval (Curve.Evaluator_Data, Axis);
                     Correction_Ratio : constant Interval := Interval_Divide (Correction, Interval_Exact (Length_Raw));
                     Correction_Bound : constant Dimensionless :=
                       (if Correction_Ratio.Valid
                        then Up (Interval_Abs_Max (Correction_Ratio) * Correction_Bounds (Order))
                        else Dimensionless'Last);
                  begin
                     if Rational_Bound >= Dimensionless'Last
                       or else Correction_Bound >= Dimensionless'Last
                       or else not Is_Finite (Rational_Bound)
                       or else not Is_Finite (Correction_Bound)
                     then
                        return Result;
                     end if;
                     declare
                        Combined_Bound : constant Dimensionless := Up (Rational_Bound + Correction_Bound);
                     begin
                        if Combined_Bound >= Dimensionless'Last or else not Is_Finite (Combined_Bound) then
                           return Result;
                        end if;
                        Result (Axis) (Order) := Combined_Bound;
                     end;
                  end;
               end if;
            end loop;
         end;

         if Order < Majorant_Order'Last then
            Advance_Numerators (Order);
            Denominator_Power := Multiply_Bernstein (Denominator_Power, Denominator_Power_Degree, Q, Q_Degree);
            Denominator_Power_Degree := Denominator_Power_Degree + Q_Degree;
         end if;
      end loop;

      Success := True;
      return Result;
   exception
      when Constraint_Error =>
         Success := False;
         return [others => [others => 0.0]];
   end Bernstein_Tangent_Range_Majorants;

   procedure Merge_Bernstein_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless; Result : in out Axis_Majorants)
   with
     Pre =>
       Curve.Retained_Tangent_Certificate.Valid
       and then Start_U >= 0.0
       and then Start_U <= End_U
       and then End_U <= 1.0;
   --  Lower each entry of Result to the Bernstein bound where Curve's retained certificate proves a smaller one.

   procedure Merge_Bernstein_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless; Result : in out Axis_Majorants)
   is
      Success : Boolean;
      Other   : Axis_Majorants;
   begin
      Other := Bernstein_Tangent_Range_Majorants (Curve, Start_U, End_U, Success);
      --  Both are valid bounds, so the smaller of each pair is.
      if Success then
         for Axis in Axis_Name loop
            for Order in Majorant_Order loop
               Result (Axis) (Order) := Dimensionless'Min (Result (Axis) (Order), Other (Axis) (Order));
            end loop;
         end loop;
      end if;
   end Merge_Bernstein_Tangent_Range_Majorants;

   function Realtime_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless) return Axis_Majorants;
   --  Compute the majorants used by the real-time tangent evaluator over a range.

   function Realtime_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless) return Axis_Majorants
   is
      Sharp  : Boolean;
      Result : Axis_Majorants;
   begin
      Result := Pole_Taylor_Tangent_Range_Majorants (Curve, Start_U, End_U, Sharp);
      --  The pole-based enclosure is the cheapest path and normally the sharpest. It is not sharp when a pole lies
      --  too close to the range for its fixed work, or when the partial fractions cancel so far that its rounding
      --  allowance shows. The Bernstein form is then consulted as well. Construction keeps the certificate when
      --  the whole curve needed it; otherwise it is reconstructed here, on a copy because Curve is constant.
      if Sharp
        or else Curve.Evaluator_Data.Kind /= Positive_Curve_Kind
        or else not (Start_U >= 0.0 and then Start_U <= End_U and then End_U <= 1.0)
      then
         return Result;
      elsif Curve.Retained_Tangent_Certificate.Valid then
         Merge_Bernstein_Tangent_Range_Majorants (Curve, Start_U, End_U, Result);
         return Result;
      end if;
      declare
         Certified : Stereographic_Curve := Curve;
         Retained  : Boolean;
      begin
         Retained := Retain_Bernstein_Tangent_Certificate (Certified);
         --  A retained certificate is a valid one, so the second test never fails.
         if Retained and then Certified.Retained_Tangent_Certificate.Valid then
            Merge_Bernstein_Tangent_Range_Majorants (Certified, Start_U, End_U, Result);
         end if;
         return Result;
      end;
   end Realtime_Tangent_Range_Majorants;

   Physical_Length_Floor : constant := 2.0**(-100);
   Physical_Length_Limit : constant := 2.0**104;
   --  The limits of the length of a positive curve, in millimetres.

   Physical_Bound_Limit : constant := 2.0**900;
   --  Largest partial bound which Bounds_On_Parameter_Range converts further to physical distance.

   function Bounds_On_Parameter_Range
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless) return Unit_Speed_Axial_Derivative_Bounds
   is
      Result : Unit_Speed_Axial_Derivative_Bounds := (others => <>);
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         return Result;
      end if;

      declare
         --  Majorants store factorial-scaled parameter derivatives:
         --
         --     Mₖ ≥ |(1/k!)·dᵏT/dUᵏ|.
         --
         --  Physical arc distance is S = L·U, so |dᵏT/dSᵏ| ≤ k!·Mₖ/Lᵏ.
         Majorants  : constant Axis_Majorants :=
           (if Start_U = 0.0 and then End_U = 1.0 and then Curve.Has_Whole_Curve_Majorants
            then Curve.Whole_Curve_Majorants
            else Realtime_Tangent_Range_Majorants (Curve, Start_U, End_U));
         Length_Raw : constant Dimensionless := Dimensionless (Curve.Evaluator_Data.Length_Value / mm);

         function Physical_Bound (Majorant : Dimensionless; Order : Positive) return Dimensionless
         with Pre => Order <= Maximum_Majorant_Order;
         --  Convert a normalized derivative majorant to physical-distance units.

         function Physical_Bound (Majorant : Dimensionless; Order : Positive) return Dimensionless is
            Value : Dimensionless := Majorant;
         begin
            --  The length of a positive curve is within these limits, as its type requires.
            if not (Length_Raw >= Physical_Length_Floor)
              or else not (Length_Raw <= Physical_Length_Limit)
              or else Majorant >= Dimensionless'Last
              or else not Is_Finite (Majorant)
            then
               return Dimensionless'Last;
            elsif Majorant = 0.0 then
               return 0.0;
            end if;
            for Factor in 1 .. Order loop
               --  Form k!·M_k/L^k one factor at a time. Dividing first for L >= 1 avoids a needless intermediate
               --  overflow; multiplying first for L < 1 preserves the upward direction. In either case a saturated
               --  sentinel is returned immediately and can never be divided back into a finite underbound.
               if Length_Raw >= 1.0 then
                  Value := Up (Value / Length_Raw);
                  --  The factor is at most four, so the product of a value within an eighth of the largest
                  --  number cannot overflow. A larger value is reported as no bound.
                  if not (abs Value <= Dimensionless'Last / 8.0) then
                     return Dimensionless'Last;
                  end if;
                  Value := Up (Value * Dimensionless (Factor));
               else
                  --  A bound beyond this limit is reported as none. Within it neither step can overflow, the
                  --  length being at least the least one of a curve.
                  if not (abs Value <= Physical_Bound_Limit) then
                     return Dimensionless'Last;
                  end if;
                  Value := Up (Value * Dimensionless (Factor));
                  --  At most four times the limit above, rounded upward, so this never fails.
                  if not (abs Value <= 16.0 * Physical_Bound_Limit) then
                     return Dimensionless'Last;
                  end if;
                  Value := Up (Value / Length_Raw);
               end if;
               if Value >= Dimensionless'Last or else not Is_Finite (Value) then
                  return Dimensionless'Last;
               end if;
            end loop;
            return Value;
         end Physical_Bound;
      begin
         for A in Axis_Name loop
            if Curve.Structurally_Constant_Axes (A) then
               --  Interval padding must not turn an algebraic zero into a visible planner bound.
               Result.Velocity (A) := 0.0;
               Result.Acceleration (A) := 0.0 / mm;
               Result.Jerk (A) := 0.0 / mm**2;
               Result.Snap (A) := 0.0 / mm**3;
               Result.Crackle (A) := 0.0 / mm**4;
            else
               Result.Velocity (A) := Majorants (A) (0);
               Result.Acceleration (A) := Physical_Bound (Majorants (A) (1), 1) / mm;
               Result.Jerk (A) := Physical_Bound (Majorants (A) (2), 2) / mm**2;
               Result.Snap (A) := Physical_Bound (Majorants (A) (3), 3) / mm**3;
               Result.Crackle (A) := Physical_Bound (Majorants (A) (4), 4) / mm**4;
            end if;
         end loop;
         return Result;
      end;
   end Bounds_On_Parameter_Range;

   Projection_Operand_Limit : constant := 2.0**400;
   --  Largest factor or term of a projected tangent bound. A bound which would need a larger one is unavailable.

   Projection_Sum_Limit : constant := 2.0**900;
   --  Largest partial sum of a projected tangent bound which another term is added to.

   function Added_Square (Sum, Term : Dimensionless) return Dimensionless
   with
     Pre  =>
       Sum >= 0.0 and then Sum <= Projection_Sum_Limit and then Term >= 0.0 and then Term <= Projection_Operand_Limit,
     Post => Added_Square'Result >= 0.0;
   --  An upper bound on Sum plus the square of Term.

   function Added_Square (Sum, Term : Dimensionless) return Dimensionless is
      Square : constant Dimensionless := Term * Term;
      Padded : constant Dimensionless := Up (Square);
   begin
      pragma Assert (Square >= 0.0 and then Square <= 2.0**800);
      pragma Assert (Padded >= 0.0 and then Padded <= 2.0**801);
      return Up (Sum + Padded);
   end Added_Square;

   function Added_Product (Sum, Left, Right : Dimensionless) return Dimensionless
   with Pre => abs Left <= Projection_Operand_Limit;
   --  An upper bound on Sum plus the product, or Dimensionless'Last, which is no bound, beyond the limits.

   function Added_Product (Sum, Left, Right : Dimensionless) return Dimensionless is
   begin
      if not (abs Sum <= Projection_Sum_Limit) or else not (abs Right <= Projection_Operand_Limit) then
         return Dimensionless'Last;
      end if;
      declare
         Product : constant Dimensionless := Left * Right;
         Padded  : constant Dimensionless := Up (Product);
      begin
         pragma Assert (abs Product <= 2.0**800);
         pragma Assert (abs Padded <= 2.0**801);
         return Up (Sum + Padded);
      end;
   end Added_Product;

   function Global_Projected_Bound
     (Ideal_Bound, Projection_Norm, Tangent_Error, Correction_Size : Dimensionless) return Dimensionless
   with Post => Global_Projected_Bound'Result >= -Dimensionless'Last;
   --  The sum of the three terms of the projected tangent bound, rounded upward: the ideal tangent's, the cache's
   --  tangent error times the norm of the projection, and the finish correction's. Dimensionless'Last, which is
   --  no bound, if a term is beyond the limit.

   function Global_Projected_Bound
     (Ideal_Bound, Projection_Norm, Tangent_Error, Correction_Size : Dimensionless) return Dimensionless is
   begin
      if not (abs Ideal_Bound <= Projection_Operand_Limit)
        or else not (abs Projection_Norm <= Projection_Operand_Limit)
        or else not (abs Tangent_Error <= Projection_Operand_Limit)
        or else not (abs Correction_Size <= Projection_Operand_Limit)
      then
         return Dimensionless'Last;
      end if;
      declare
         Error_Product      : constant Dimensionless := Projection_Norm * Tangent_Error;
         Cache_Error        : constant Dimensionless := Up (Error_Product);
         Correction_Product : constant Dimensionless := (693.0 / 256.0) * Correction_Size;
         Correction_Bound   : constant Dimensionless := Up (Correction_Product);
      begin
         pragma Assert (abs Error_Product <= 2.0**800);
         pragma Assert (abs Cache_Error <= 2.0**801);
         pragma Assert (abs Correction_Product <= 2.0**402);
         pragma Assert (abs Correction_Bound <= 2.0**403);
         declare
            Errors : constant Dimensionless := Up (Cache_Error + Correction_Bound);
         begin
            pragma Assert (abs Errors <= 2.0**803);
            return Up (Ideal_Bound + Errors);
         end;
      end;
   end Global_Projected_Bound;

   function Projected_Bound_On_Parameter_Range
     (Curve        : Stereographic_Curve;
      Start_U      : Dimensionless;
      End_U        : Dimensionless;
      Coefficients : Projection_Coefficients) return Curvature
   is
      Frame_Square_Sum      : Dimensionless := 0.0;
      Projection_Square_Sum : Dimensionless := 0.0;
      Correction_Numerator  : Interval := Interval_Exact (0.0);
      Component_Bound       : Dimensionless := 0.0;

   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind or else Is_Zero_Projection (Coefficients) then
         return 0.0 / mm;
      end if;
      if (for all Axis in Axis_Name => Curve.Structurally_Constant_Axes (Axis) or else Coefficients (Axis) = 0.0 / mm)
      then
         return 0.0 / mm;
      end if;

      --  For the authoritative ideal unit tangent, |p·F·T| <= ||F'·p||. The retained cache differs from that tangent
      --  by at most Certified_Tangent_Error in Euclidean norm, and the endpoint correction adds
      --  (R/L)·H'(U). Bound all three terms independently while preserving cancellation in both dot products.
      for Component in Frame_Component_Index loop
         pragma Loop_Invariant (Frame_Square_Sum >= 0.0);
         declare
            Local_Projection : Interval := Interval_Exact (0.0);
         begin
            for Axis in Axis_Name loop
               if not Curve.Structurally_Constant_Axes (Axis) then
                  Local_Projection :=
                    Interval_Add
                      (Local_Projection,
                       Interval_Multiply
                         (Interval_Exact (Dimensionless (Coefficients (Axis) / (1.0 / mm))),
                          Interval_Exact (Curve.Frame (Component) (Axis))));
               end if;
            end loop;
            declare
               Bound : constant Dimensionless := Interval_Abs_Max (Local_Projection);
            begin
               if not (Bound <= Projection_Operand_Limit) or else not (Frame_Square_Sum <= Projection_Sum_Limit) then
                  --  Too large to square and add: no bound.
                  return Curvature'Last;
               end if;
               Frame_Square_Sum := Added_Square (Frame_Square_Sum, Bound);
            end;
         end;
      end loop;

      declare
         Range_Majorants : constant Axis_Majorants :=
           (if Start_U = 0.0 and then End_U = 1.0 and then Curve.Has_Whole_Curve_Majorants
            then Curve.Whole_Curve_Majorants
            else Realtime_Tangent_Range_Majorants (Curve, Start_U, End_U));
      begin
         for Axis in Axis_Name loop
            pragma Loop_Invariant (Projection_Square_Sum >= 0.0);
            if not Curve.Structurally_Constant_Axes (Axis) then
               declare
                  Signed_Coefficient : constant Dimensionless := Dimensionless (Coefficients (Axis) / (1.0 / mm));
                  Coefficient        : constant Dimensionless := abs Signed_Coefficient;
                  Majorant           : constant Dimensionless := Range_Majorants (Axis) (0);
               begin
                  if Coefficient /= 0.0 then
                     if not (Coefficient <= Projection_Operand_Limit)
                       or else not (Projection_Square_Sum <= Projection_Sum_Limit)
                     then
                        --  Too large to square and add: no bound.
                        return Curvature'Last;
                     end if;
                     Projection_Square_Sum := Added_Square (Projection_Square_Sum, Coefficient);
                     Correction_Numerator :=
                       Interval_Add
                         (Correction_Numerator,
                          Interval_Multiply
                            (Interval_Exact (Signed_Coefficient),
                             Finish_Correction_Interval (Curve.Evaluator_Data, Axis)));
                     if Majorant >= Dimensionless'Last then
                        Component_Bound := Dimensionless'Last;
                     elsif Component_Bound < Dimensionless'Last and then Majorant /= 0.0 then
                        Component_Bound := Added_Product (Component_Bound, Coefficient, Majorant);
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end;

      declare
         Correction_Projection : constant Interval :=
           Interval_Divide
             (Correction_Numerator, Interval_Exact (Dimensionless (Curve.Evaluator_Data.Length_Value / mm)));
         Ideal_Bound           : constant Dimensionless := Certified_Upper_Square_Root (Frame_Square_Sum);
         Projection_Norm       : constant Dimensionless := Certified_Upper_Square_Root (Projection_Square_Sum);
         Global_Result         : constant Dimensionless :=
           Global_Projected_Bound
             (Ideal_Bound, Projection_Norm, Curve.Certified_Tangent_Error, Interval_Abs_Max (Correction_Projection));
         Result                : constant Dimensionless := Dimensionless'Min (Global_Result, Component_Bound);
      begin
         return (if Result >= Dimensionless'Last or else not Is_Finite (Result) then Curvature'Last else Result / mm);
      end;
   end Projected_Bound_On_Parameter_Range;

   ---------------------------------------------------------------------------
   --  Rational realtime evaluator
   ---------------------------------------------------------------------------
   --
   --  In the polynomial chart coordinate the inverse-stereographic tangent is a degree-sixteen rational function.
   --  Construction factors its positive denominator and stores a real partial-fraction expansion. The poles and
   --  residues are then transformed through the same Möbius map as the ideal chart, so realtime evaluation works
   --  directly in normalized physical distance.

   function Complex_Pair_Real_Slot
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index) return Rational_Degree_Slot is
   begin
      return Rational_Degree_Slot (Cache.Real_Pole_Count + 2 * Natural (Pair) - 1);
   end Complex_Pair_Real_Slot;

   function Complex_Pair_Imaginary_Slot
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index) return Rational_Degree_Slot is
   begin
      return Complex_Pair_Real_Slot (Cache, Pair) + 1;
   end Complex_Pair_Imaginary_Slot;

   function Stable_Log_One_Plus (Value : Dimensionless) return Dimensionless is
      Odd_Reciprocals              : constant array (Natural range 0 .. 9) of Dimensionless :=
        [1.0, 1.0 / 3.0, 1.0 / 5.0, 1.0 / 7.0, 1.0 / 9.0, 1.0 / 11.0, 1.0 / 13.0, 1.0 / 15.0, 1.0 / 17.0, 1.0 / 19.0];
      Reduced, Reduced_Square, Sum : Dimensionless;
   begin
      --  log(1 + x) = 2·atanh(x/(2 + x)). The short odd series
      --
      --     log(1 + x) = 2·Σₖ₌₀⁹ z²ᵏ⁺¹/(2k + 1),   z = x/(2 + x),
      --
      --  avoids losing the small displacement when a pole is far from the curve. It is summed by Horner's rule in
      --  z², whose terms all have the same sign. With |x| ≤ 1/8, |z| ≤ 1/15 and the sum stays below two.
      Reduced := Value / (2.0 + Value);
      Reduced_Square := Reduced * Reduced;
      Sum := Odd_Reciprocals (9);
      for Order in reverse 0 .. 8 loop
         pragma Loop_Invariant (Sum >= 0.0 and then Sum <= 2.0);
         pragma Assert (Odd_Reciprocals (Order) >= 0.0 and then Odd_Reciprocals (Order) <= 1.0);
         Sum := Sum * Reduced_Square + Odd_Reciprocals (Order);
      end loop;
      return 2.0 * (Reduced * Sum);
   end Stable_Log_One_Plus;

   function Stable_Real_Log_Ratio (Pole, Normalized_Distance : Dimensionless) return Dimensionless is
   begin
      if Normalized_Distance = 0.0 then
         return 0.0;
      elsif Normalized_Distance <= 0.125 * abs Pole then
         declare
            Change : constant Dimensionless := -Normalized_Distance / Pole;
         begin
            --  The comparison above already says that the change is at most an eighth. Testing the quotient as
            --  well costs nothing and spares the proof an argument about the rounding of the two.
            if abs Change <= 0.125 then
               return Stable_Log_One_Plus (Change);
            end if;
         end;
      end if;

      --  Near a pole just outside an endpoint, -U/Pole can round to -1 even though the exact log argument is
      --  positive. The difference-of-logs form preserves that positive distance. The small-relative-change branch
      --  above avoids cancellation between these logarithms for distant poles.
      Stereographic_Kernel.Elementary.Lemma_Log_Is_Bounded (abs (Pole - Normalized_Distance));
      Stereographic_Kernel.Elementary.Lemma_Log_Is_Bounded (abs Pole);
      declare
         Point_Log : constant Dimensionless := Dimensionless_Math.Log (abs (Pole - Normalized_Distance));
         Base_Log  : constant Dimensionless := Dimensionless_Math.Log (abs Pole);
      begin
         pragma Assert (abs Point_Log <= Stereographic_Kernel.Elementary.Log_Magnitude_Max);
         pragma Assert (abs Base_Log <= Stereographic_Kernel.Elementary.Log_Magnitude_Max);
         return Point_Log - Base_Log;
      end;
   end Stable_Real_Log_Ratio;

   function Stable_Complex_Log_Ratio
     (Pole_Real, Pole_Imaginary, Normalized_Distance : Dimensionless) return Dimensionless
   is
      Point_Absolute_Real : constant Dimensionless := abs (Normalized_Distance - Pole_Real);
      Base                : constant Dimensionless := Pole_Real * Pole_Real + Pole_Imaginary * Pole_Imaginary;
      Point               : constant Dimensionless :=
        Point_Absolute_Real * Point_Absolute_Real + Pole_Imaginary * Pole_Imaginary;
   begin
      --  A pole within the magnitude limit and clear of the unit interval has squares which neither overflow nor
      --  underflow, so neither sum of squares needs scaling, and neither is zero.
      pragma Assert (Base >= 2.0**(-200) and then Base <= 2.0**201);
      pragma Assert (Point >= 2.0**(-200) and then Point <= 2.0**203);
      declare
         Change : constant Dimensionless := Normalized_Distance * (Normalized_Distance - 2.0 * Pole_Real) / Base;
      begin
         --  The relative change U·(U - 2A)/(A² + B²) while it is small, and otherwise the quotient of the two sums
         --  of squares, neither of which can cancel.
         if abs Change <= 0.125 then
            return Stable_Log_One_Plus (Change);
         else
            Stereographic_Kernel.Elementary.Lemma_Log_Is_Bounded (Point / Base);
            return Dimensionless_Math.Log (Point / Base);
         end if;
      end;
   end Stable_Complex_Log_Ratio;

   function Stable_Complex_Angle_Delta
     (Pole_Real, Pole_Imaginary, Normalized_Distance : Dimensionless) return Dimensionless
   is
      --  The cross and dot products between (-A,-B) and (U-A,-B).
      Cross : constant Dimensionless := Pole_Imaginary * Normalized_Distance;
      Dot   : constant Dimensionless :=
        Pole_Imaginary * Pole_Imaginary + Pole_Real * (Pole_Real - Normalized_Distance);
   begin
      pragma Assert (if Normalized_Distance = 0.0 then Cross = 0.0 and then Dot >= 2.0**(-200));
      --  Both products vanish only where the point coincides with the pole, which a pole clear of the unit interval
      --  rules out. The test spares the proof that argument, which rounding makes a long one.
      if Cross = 0.0 and then Dot = 0.0 then
         return 0.0;
      end if;
      Stereographic_Kernel.Elementary.Lemma_Arctan_Is_Bounded (Cross, Dot);
      return Dimensionless_Math.Arctan (Cross, Dot);
   end Stable_Complex_Angle_Delta;

   Slot_Term_Limit : constant := 2.0**111;
   --  Largest magnitude which one real pole or one conjugate pair adds to a component of the displacement: a
   --  residue times a logarithm of a ratio, and for a pair another residue times twice an angle.

   function Within_Slots (Value : Dimensionless_Axis_Vector; Slots : Natural) return Boolean
   is (for all Axis in Axis_Name => abs Value (Axis) <= Dimensionless (Slots) * Slot_Term_Limit)
   with Pre => Slots <= Maximum_Rational_Degree + 2;
   --  Whether every component is within the given number of term limits. A displacement is within one more of
   --  them than the poles and pairs it includes, the first being for the constant tangent.

   procedure Add_Real_Pole
     (Result       : in out Dimensionless_Axis_Vector;
      Residue      : Dimensionless_Axis_Vector;
      Primitive    : Dimensionless;
      Slots_Before : Natural)
   with
     Pre  =>
       Slots_Before <= Maximum_Rational_Degree
       and then abs Primitive <= Log_Ratio_Limit
       and then (for all Axis in Axis_Name => abs Residue (Axis) <= Residue_Magnitude_Limit)
       and then Within_Slots (Result, Slots_Before),
     Post =>
       Within_Slots (Result, Slots_Before + 1)
       and then (if Primitive = 0.0 then (for all Axis in Axis_Name => Result (Axis) = Result'Old (Axis)));
   --  Add r·log(1 - U/p) for one real pole to every component.

   procedure Add_Real_Pole
     (Result       : in out Dimensionless_Axis_Vector;
      Residue      : Dimensionless_Axis_Vector;
      Primitive    : Dimensionless;
      Slots_Before : Natural)
   is
      Before : constant Dimensionless_Axis_Vector := Result
      with Ghost;
   begin
      pragma Assert (for all Axis in Axis_Name => abs Before (Axis) <= Dimensionless (Slots_Before) * Slot_Term_Limit);
      for Axis in Axis_Name loop
         pragma Assert (Result (Axis) = Before (Axis));
         pragma Assert (abs Result (Axis) <= Dimensionless (Slots_Before) * Slot_Term_Limit);
         pragma Assert (abs Residue (Axis) <= Residue_Magnitude_Limit);
         declare
            Term : constant Dimensionless := Primitive * Residue (Axis);
            Sum  : constant Dimensionless := Result (Axis) + Term;
         begin
            pragma Assert (abs Term <= Slot_Term_Limit);
            pragma Assert (abs Sum <= Dimensionless (Slots_Before + 1) * Slot_Term_Limit);
            pragma Assert (if Primitive = 0.0 then Sum = Before (Axis));
            Result (Axis) := Sum;
         end;
         pragma
           Loop_Invariant
             (for all Other in Axis_Name =>
                (if Other <= Axis
                 then abs Result (Other) <= Dimensionless (Slots_Before + 1) * Slot_Term_Limit
                 else Result (Other) = Before (Other)));
         pragma
           Loop_Invariant (if Primitive = 0.0 then (for all Other in Axis_Name => Result (Other) = Before (Other)));
      end loop;
   end Add_Real_Pole;
   pragma Inline (Add_Real_Pole);

   procedure Add_Pole_Pair
     (Result                          : in out Dimensionless_Axis_Vector;
      Real_Residue, Imaginary_Residue : Dimensionless_Axis_Vector;
      Log_Ratio, Angle_Delta          : Dimensionless;
      Slots_Before                    : Natural)
   with
     Pre  =>
       Slots_Before <= Maximum_Rational_Degree
       and then abs Log_Ratio <= Log_Ratio_Limit
       and then abs Angle_Delta <= Stereographic_Kernel.Elementary.Arctan_Magnitude_Max
       and then (for all Axis in Axis_Name => abs Real_Residue (Axis) <= Residue_Magnitude_Limit)
       and then (for all Axis in Axis_Name => abs Imaginary_Residue (Axis) <= Residue_Magnitude_Limit)
       and then Within_Slots (Result, Slots_Before),
     Post =>
       Within_Slots (Result, Slots_Before + 1)
       and then
         (if Log_Ratio = 0.0 and then Angle_Delta = 0.0
          then (for all Axis in Axis_Name => Result (Axis) = Result'Old (Axis)));
   --  Add one conjugate pair's primitive to every component. For p = A + iB and r = R + iI it is
   --
   --     R·log(((U - A)² + B²)/(A² + B²)) - 2·I·Δarg(U - p).

   procedure Add_Pole_Pair
     (Result                          : in out Dimensionless_Axis_Vector;
      Real_Residue, Imaginary_Residue : Dimensionless_Axis_Vector;
      Log_Ratio, Angle_Delta          : Dimensionless;
      Slots_Before                    : Natural)
   is
      Before : constant Dimensionless_Axis_Vector := Result
      with Ghost;
   begin
      pragma Assert (for all Axis in Axis_Name => abs Before (Axis) <= Dimensionless (Slots_Before) * Slot_Term_Limit);
      for Axis in Axis_Name loop
         pragma Assert (Result (Axis) = Before (Axis));
         pragma Assert (abs Result (Axis) <= Dimensionless (Slots_Before) * Slot_Term_Limit);
         pragma Assert (abs Real_Residue (Axis) <= Residue_Magnitude_Limit);
         pragma Assert (abs Imaginary_Residue (Axis) <= Residue_Magnitude_Limit);
         declare
            Log_Term   : constant Dimensionless := Real_Residue (Axis) * Log_Ratio;
            Angle_Term : constant Dimensionless := Imaginary_Residue (Axis) * (2.0 * Angle_Delta);
            Partial    : constant Dimensionless := Result (Axis) + Log_Term;
            Sum        : constant Dimensionless := Partial - Angle_Term;
         begin
            --  The two terms are within 1492·2¹⁰⁰ and 6.5·2¹⁰⁰, which together are below the term limit 2048·2¹⁰⁰.
            pragma Assert (abs Log_Term <= 1_492.0 * 2.0**100);
            pragma Assert (abs Angle_Term <= 8.0 * 2.0**100);
            pragma Assert (abs Partial <= Dimensionless (Slots_Before) * Slot_Term_Limit + 1_492.0 * 2.0**100);
            pragma Assert (abs Sum <= Dimensionless (Slots_Before) * Slot_Term_Limit + 1_500.0 * 2.0**100);
            pragma Assert (abs Sum <= Dimensionless (Slots_Before + 1) * Slot_Term_Limit);
            pragma
              Assert (if Log_Ratio = 0.0 and then Angle_Delta = 0.0 then Log_Term = 0.0 and then Angle_Term = 0.0);
            pragma Assert (if Log_Ratio = 0.0 and then Angle_Delta = 0.0 then Sum = Before (Axis));
            Result (Axis) := Sum;
         end;
         pragma
           Loop_Invariant
             (for all Other in Axis_Name =>
                (if Other <= Axis
                 then abs Result (Other) <= Dimensionless (Slots_Before + 1) * Slot_Term_Limit
                 else Result (Other) = Before (Other)));
         pragma
           Loop_Invariant
             (if Log_Ratio = 0.0 and then Angle_Delta = 0.0
              then (for all Other in Axis_Name => Result (Other) = Before (Other)));
      end loop;
   end Add_Pole_Pair;
   pragma Inline (Add_Pole_Pair);

   procedure Lemma_Displacement_Is_Bounded (Value : Dimensionless_Axis_Vector; Slots : Natural)
   with
     Ghost,
     Pre  => Slots <= Maximum_Rational_Degree + 1 and then Within_Slots (Value, Slots),
     Post => (for all Axis in Axis_Name => abs Value (Axis) <= Displacement_Limit);

   procedure Lemma_Displacement_Is_Bounded (Value : Dimensionless_Axis_Vector; Slots : Natural) is
   begin
      pragma Assert (Dimensionless (Slots) <= 17.0);
      pragma Assert (Dimensionless (Slots) * Slot_Term_Limit <= Displacement_Limit);
      pragma Assert (for all Axis in Axis_Name => abs Value (Axis) <= Displacement_Limit);
   end Lemma_Displacement_Is_Bounded;

   function Evaluate_Rational_Displacement
     (Cache : Rational_Antiderivative; Normalized_Distance : Dimensionless) return Dimensionless_Axis_Vector
   is
      Result : Dimensionless_Axis_Vector :=
        [for Axis in Axis_Name => Normalized_Distance * Cache.Constant_Tangent (Axis)];
   begin
      pragma Assert (for all Axis in Axis_Name => abs Cache.Constant_Tangent (Axis) <= Residue_Magnitude_Limit);
      pragma Assert (Within_Slots (Result, 1));

      --  Integrating c + Σr/(U - p) from 0 to U gives
      --
      --     c·U + Σr·log(1 - U/p).
      for Index in 1 .. Cache.Real_Pole_Count loop
         pragma Loop_Invariant (Within_Slots (Result, Index));
         pragma Loop_Invariant (if Normalized_Distance = 0.0 then (for all Axis in Axis_Name => Result (Axis) = 0.0));
         pragma Assert (Slot_Is_Tame (Cache.Pole_Slots (Index)));
         pragma Assert (Is_Clear_Of_Unit_Interval (Cache.Pole_Slots (Index).Pole_Component));
         Add_Real_Pole
           (Result,
            Cache.Pole_Slots (Index).Residue_Component,
            Stable_Real_Log_Ratio (Cache.Pole_Slots (Index).Pole_Component, Normalized_Distance),
            Index);
      end loop;
      pragma Assert (Within_Slots (Result, Cache.Real_Pole_Count + 1));

      for Pair in 1 .. Cache.Pair_Count loop
         pragma Loop_Invariant (Within_Slots (Result, Cache.Real_Pole_Count + Pair));
         pragma Loop_Invariant (if Normalized_Distance = 0.0 then (for all Axis in Axis_Name => Result (Axis) = 0.0));
         pragma Assert (Slot_Is_Tame (Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair - 1)));
         pragma Assert (Slot_Is_Tame (Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair)));
         pragma
           Assert
             (Pair_Is_Clear
                (Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair - 1).Pole_Component,
                 Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair).Pole_Component));
         declare
            Real_Slot      : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair)));
            Imaginary_Slot : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair)));
            A              : constant Dimensionless := Real_Slot.Pole_Component;
            B              : constant Dimensionless := Imaginary_Slot.Pole_Component;
         begin
            Add_Pole_Pair
              (Result,
               Real_Slot.Residue_Component,
               Imaginary_Slot.Residue_Component,
               Stable_Complex_Log_Ratio (A, B, Normalized_Distance),
               Stable_Complex_Angle_Delta (A, B, Normalized_Distance),
               Cache.Real_Pole_Count + Pair);
         end;
      end loop;
      pragma Assert (Within_Slots (Result, Cache.Real_Pole_Count + Cache.Pair_Count + 1));
      Lemma_Displacement_Is_Bounded (Result, Cache.Real_Pole_Count + Cache.Pair_Count + 1);
      return Result;
   end Evaluate_Rational_Displacement;

   function Endpoint_Smootherstep (Normalized_Distance : Dimensionless) return Dimensionless
   with
     Post =>
       abs Endpoint_Smootherstep'Result <= 64.0
       and then (if Normalized_Distance <= 0.0 then Endpoint_Smootherstep'Result = 0.0)
       and then (if Normalized_Distance >= 1.0 then Endpoint_Smootherstep'Result = 1.0);
   --  Evaluate the endpoint-flattening smootherstep correction. It lies between zero and one; the proof of the
   --  evaluator needs no more than the coarse bound stated here.

   function Endpoint_Smootherstep (Normalized_Distance : Dimensionless) return Dimensionless is
      U          : constant Dimensionless :=
        (if Normalized_Distance <= 0.5 then Normalized_Distance else 1.0 - Normalized_Distance);
      Lower_Half : Dimensionless;
   begin
      --  This is the degree-eleven generalized smoothstep
      --
      --     462 U^6 - 1980 U^7 + 3465 U^8 - 3080 U^9 + 1386 U^10 - 252 U^11.
      --
      --  Its first five derivatives vanish at both endpoints. Evaluate only on [0, 1/2] and use symmetry for the
      --  upper half, avoiding the severe cancellation of the power form near one.
      if Normalized_Distance <= 0.0 then
         return 0.0;
      elsif Normalized_Distance >= 1.0 then
         return 1.0;
      end if;
      declare
         U2 : constant Dimensionless := U * U;
         U6 : constant Dimensionless := U2 * (U2 * U2);
      begin
         Lower_Half := U6 * (462.0 + U * (-1_980.0 + U * (3_465.0 + U * (-3_080.0 + U * (1_386.0 - 252.0 * U)))));
      end;
      return (if Normalized_Distance <= 0.5 then Lower_Half else 1.0 - Lower_Half);
   end Endpoint_Smootherstep;

   function Evaluate_Uncorrected_Rational_Point
     (Evaluator : Stereographic_Curve_Evaluator; Normalized_Distance : Dimensionless) return Position
   is
      Displacement : constant Dimensionless_Axis_Vector :=
        Evaluate_Rational_Displacement (Evaluator.Antiderivative_Cache, Normalized_Distance);
      Result       : Position := [others => 0.0 * mm];
   begin
      for Axis in Axis_Name loop
         pragma
           Loop_Invariant
             (for all Other in Axis_Name =>
                (if Other < Axis
                 then
                   abs Result (Other) <= Uncorrected_Point_Limit
                   and then (if Normalized_Distance = 0.0 then Result (Other) = Evaluator.Start_Point (Other))));
         pragma Assert (abs Displacement (Axis) <= Displacement_Limit);
         pragma Assert (abs Evaluator.Start_Point (Axis) <= Coordinate_Limit);
         Result (Axis) := Evaluator.Start_Point (Axis) + Evaluator.Length_Value * Displacement (Axis);
      end loop;
      return Result;
   end Evaluate_Uncorrected_Rational_Point;

   function Evaluate_Rational_Point
     (Evaluator : Stereographic_Curve_Evaluator; Normalized_Distance : Dimensionless) return Position
   is
      Uncorrected : constant Position := Evaluate_Uncorrected_Rational_Point (Evaluator, Normalized_Distance);
      Blend       : constant Dimensionless := Endpoint_Smootherstep (Normalized_Distance);
      Complement  : constant Dimensionless := 1.0 - Blend;
      Result      : Position := [others => 0.0 * mm];
   begin
      --  In exact arithmetic this is
      --
      --     Uncorrected(U) + H(U)·(Finish_Point - Uncorrected(1)).
      --
      --  The barycentric form below retains the same continuous endpoint-flat correction while anchoring both ends
      --  in floating-point arithmetic. At U = 0 only Uncorrected(0) remains; at U = 1 both differences from the
      --  stored bit-exact Uncorrected(1) vanish and only Finish_Point remains. This avoids asking one correction
      --  value to bridge a cancellation gap which may contain no representable floating-point number.
      for Axis in Axis_Name loop
         pragma
           Loop_Invariant
             (for all Other in Axis_Name =>
                (if Other < Axis
                 then
                   (if Normalized_Distance = 0.0 then Result (Other) = Evaluator.Start_Point (Other))
                   and then
                     (if Normalized_Distance = 1.0
                        and then Uncorrected (Other) = Evaluator.Uncorrected_Finish_Point (Other)
                      then Result (Other) = Evaluator.Finish_Point (Other))));
         pragma Assert (abs Uncorrected (Axis) <= Uncorrected_Point_Limit);
         pragma
           Assert
             (abs Evaluator.Finish_Point (Axis) <= Coordinate_Limit
              and then abs Evaluator.Uncorrected_Finish_Point (Axis) <= Coordinate_Limit);
         Result (Axis) :=
           (if Evaluator.Finish_Point (Axis) = Evaluator.Uncorrected_Finish_Point (Axis)
            then Uncorrected (Axis)
            else
              Complement * Uncorrected (Axis) + Blend * Evaluator.Finish_Point (Axis)
              + Blend * (Uncorrected (Axis) - Evaluator.Uncorrected_Finish_Point (Axis)));
      end loop;
      return Result;
   end Evaluate_Rational_Point;

   function Cached_Evaluator_Tangent_Derivative_At
     (Cache : Rational_Antiderivative;
      U     : Dimensionless;
      Order : Endpoint_Tangent_Derivative_Order;
      Value : out Interval_Position_Scale) return Boolean
   with Side_Effects;
   --  Outward-enclose a cached tangent derivative at one normalized distance.

   function Cached_Evaluator_Tangent_Derivative_At
     (Cache : Rational_Antiderivative;
      U     : Dimensionless;
      Order : Endpoint_Tangent_Derivative_Order;
      Value : out Interval_Position_Scale) return Boolean
   is

      function Unbounded_Interval return Interval
      is ((Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False));

      Signed_Factor : constant Dimensionless := (if Order mod 2 = 0 then Factorial (Order) else -Factorial (Order));
   begin
      if Cache.Real_Pole_Count + 2 * Cache.Pair_Count > Maximum_Rational_Degree then
         --  The cache of no evaluator has more poles than slots.
         Value := [others => Unbounded_Interval];
         return False;
      end if;
      Value :=
        (if Order = 0
         then [for Axis in Axis_Name => Interval_Exact (Cache.Constant_Tangent (Axis))]
         else [others => Interval_Exact (0.0)]);
      for Index in 1 .. Cache.Real_Pole_Count loop
         declare
            Slot       : Rational_Pole_Slot renames Cache.Pole_Slots (Index);
            Reciprocal : constant Interval :=
              Interval_Divide
                (Interval_Exact (1.0), Interval_Subtract (Interval_Exact (U), Interval_Exact (Slot.Pole_Component)));
            Power      : Interval := Interval_Exact (1.0);
         begin
            for Exponent in 1 .. Order + 1 loop
               Power := Interval_Multiply (Power, Reciprocal);
            end loop;
            for Axis in Axis_Name loop
               Value (Axis) :=
                 Interval_Add
                   (Value (Axis),
                    Interval_Multiply
                      (Interval_Exact (Signed_Factor),
                       Interval_Multiply (Interval_Exact (Slot.Residue_Component (Axis)), Power)));
            end loop;
         end;
      end loop;

      for Pair in 1 .. Cache.Pair_Count loop
         declare
            Real_Slot        : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair)));
            Imaginary_Slot   : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair)));
            Reciprocal       : constant Interval_Complex :=
              Complex_Reciprocal (U, Real_Slot.Pole_Component, Imaginary_Slot.Pole_Component);
            Reciprocal_Power : Interval_Complex :=
              (Real_Part => Interval_Exact (1.0), Imaginary_Part => Interval_Exact (0.0));
         begin
            for Exponent in 1 .. Order + 1 loop
               Reciprocal_Power := Complex_Multiply (Reciprocal_Power, Reciprocal);
            end loop;
            for Axis in Axis_Name loop
               Value (Axis) :=
                 Interval_Add
                   (Value (Axis),
                    Interval_Multiply
                      (Interval_Exact (2.0 * Signed_Factor),
                       Interval_Subtract
                         (Interval_Multiply
                            (Interval_Exact (Real_Slot.Residue_Component (Axis)), Reciprocal_Power.Real_Part),
                          Interval_Multiply
                            (Interval_Exact (Imaginary_Slot.Residue_Component (Axis)),
                             Reciprocal_Power.Imaginary_Part))));
            end loop;
         end;
      end loop;
      return (for all Axis in Axis_Name => Value (Axis).Valid);
   end Cached_Evaluator_Tangent_Derivative_At;

   function Measure_Evaluator_Endpoint_Jet_Error
     (Evaluator            : Stereographic_Curve_Evaluator;
      Requested_Start_Jet  : Endpoint_Tangent_Jet;
      Requested_Finish_Jet : Endpoint_Tangent_Jet;
      Error_Bounds         : out Endpoint_Jet_Error_Bounds) return Boolean
   is
      function Expected_Derivative
        (Jet : Endpoint_Tangent_Jet; Axis : Axis_Name; Order : Endpoint_Tangent_Derivative_Order) return Interval;

      function Measure_Endpoint
        (Jet : Endpoint_Tangent_Jet; U : Dimensionless; Bounds : out Endpoint_Jet_Error_Bounds) return Boolean
      with Side_Effects;

      function Expected_Derivative
        (Jet : Endpoint_Tangent_Jet; Axis : Axis_Name; Order : Endpoint_Tangent_Derivative_Order) return Interval
      is
         Length_Raw : constant Interval := Interval_Exact (Dimensionless (Evaluator.Length_Value / mm));
         Result     : Interval;
      begin
         case Order is
            when 0 =>
               Result := Interval_Exact (Jet.Tangent (Axis));

            when 1 =>
               Result := Interval_Exact (Dimensionless (Jet.Tangent_Derivative_1 (Axis) * mm));

            when 2 =>
               Result := Interval_Exact (Dimensionless (Jet.Tangent_Derivative_2 (Axis) * mm**2));

            when 3 =>
               Result := Interval_Exact (Dimensionless (Jet.Tangent_Derivative_3 (Axis) * mm**3));
         end case;
         for Exponent in 1 .. Order loop
            Result := Interval_Multiply (Result, Length_Raw);
         end loop;
         return Result;
      end Expected_Derivative;

      function Measure_Endpoint
        (Jet : Endpoint_Tangent_Jet; U : Dimensionless; Bounds : out Endpoint_Jet_Error_Bounds) return Boolean
      is
         Actual    : Interval_Position_Scale;
         Evaluated : Boolean;
      begin
         Bounds := [others => 0.0];
         for Order in Endpoint_Tangent_Derivative_Order loop
            Evaluated := Cached_Evaluator_Tangent_Derivative_At (Evaluator.Antiderivative_Cache, U, Order, Actual);
            if not Evaluated then
               return False;
            end if;
            declare
               Square_Sum : Interval := Interval_Exact (0.0);
            begin
               for Axis in Axis_Name loop
                  declare
                     Difference : constant Interval :=
                       Interval_Subtract (Actual (Axis), Expected_Derivative (Jet, Axis, Order));
                     Axis_Error : constant Dimensionless := Interval_Abs_Max (Difference);
                  begin
                     if not Difference.Valid or else not Is_Finite (Axis_Error) then
                        return False;
                     end if;
                     Square_Sum :=
                       Interval_Add
                         (Square_Sum, Interval_Multiply (Interval_Exact (Axis_Error), Interval_Exact (Axis_Error)));
                  end;
               end loop;
               if not Square_Sum.Valid or else Square_Sum.Upper < 0.0 then
                  return False;
               end if;
               Bounds (Order) := Certified_Upper_Square_Root (Dimensionless'Max (0.0, Square_Sum.Upper));
               if not Is_Finite (Bounds (Order)) or else Bounds (Order) >= Dimensionless'Last then
                  return False;
               end if;
            end;
         end loop;
         return True;
      end Measure_Endpoint;

      Start_Bounds, Finish_Bounds : Endpoint_Jet_Error_Bounds := [others => Dimensionless'Last];
      Measured                    : Boolean := Evaluator.Kind = Positive_Curve_Kind;
   begin
      if Measured then
         Measured := Measure_Endpoint (Requested_Start_Jet, 0.0, Start_Bounds);
      end if;
      if Measured then
         Measured := Measure_Endpoint (Requested_Finish_Jet, 1.0, Finish_Bounds);
      end if;
      if not Measured then
         Error_Bounds := [others => Dimensionless'Last];
         return False;
      end if;
      for Order in Endpoint_Tangent_Derivative_Order loop
         Error_Bounds (Order) := Dimensionless'Max (Start_Bounds (Order), Finish_Bounds (Order));
      end loop;
      return True;
   end Measure_Evaluator_Endpoint_Jet_Error;

   ----------------------------------------------------------------------------------------------------------------
   --  Sampled tangent certificate
   --
   --  Sampled_Tangent holds the certificate itself, with the reasoning behind it and the proof that its bound
   --  holds. What remains here are the checks which need the evaluator's primitives.
   ----------------------------------------------------------------------------------------------------------------

   function Tangent_Sample_Node (Index : Tangent_Sample_Index) return Dimensionless
   is (Stereographic_Kernel.Sample_Nodes.Table.Samples (Index).Node);

   function Sampled_Realtime_Tangent_Error
     (Curve             : Stereographic_Curve;
      Final_Cache       : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Evidence          : out Stereographic_Kernel.Certified_Evaluator.Certificate) return Tangent_Certificate_Result
   is
      No_Evidence : Stereographic_Kernel.Certified_Evaluator.Certificate;
   begin
      Evidence := No_Evidence;
      if not Rational_Antiderivative_Is_Well_Formed (Final_Cache)
        or else not (Denominator_Scale > 0.0
                     and then Denominator_Scale <= Stereographic_Kernel.Sampled_Tangent.Magnitude_Limit)
        or else Curve.Evaluator_Data.Length_Value <= 0.0 * mm
      then
         return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
      end if;

      declare
         --  The geometry certificate holds the frame and the chart of Curve; Create_Blend checks that it does.
         Sampled : constant Stereographic_Kernel.Certified_Evaluator.Certification :=
           Stereographic_Kernel.Certified_Evaluator.Certify
             (Curve.Geometry, Curve.Warp_Factor, Final_Cache, Denominator_Scale);
      begin
         Evidence := Sampled.Evidence;
         case Sampled.Outcome is
            when Stereographic_Kernel.Sampled_Tangent.Certified     =>
               if not (Sampled.Error_Bound >= 0.0 and then Is_Finite (Sampled.Error_Bound)) then
                  --  Never: a certified bound is a finite number which is not negative.
                  return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
               end if;
               return (Status => Tangent_Was_Certified, Error_Bound => Sampled.Error_Bound);

            when Stereographic_Kernel.Sampled_Tangent.Not_Certified =>
               return (Status => Tangent_Could_Not_Be_Certified);

            when Stereographic_Kernel.Sampled_Tangent.Unsafe        =>
               return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
         end case;
      end;
   end Sampled_Realtime_Tangent_Error;

   --  The final representation certificate compares two rational tangent fields in Bernstein form. Degree sixteen
   --  is enough for either numerator or denominator; cross multiplication therefore needs degree thirty-two.
   subtype Certificate_Polynomial_Index is Natural range 0 .. 2 * Maximum_Rational_Degree;
   subtype Certificate_Polynomial is Interval_Polynomial (Certificate_Polynomial_Index);
   type Certificate_Centres is array (Certificate_Polynomial_Index) of Dimensionless;
   type Quadratic_Factor_Coefficients is array (Natural range 0 .. 2) of Interval;

   type Rational_Factor is record
      Degree        : Positive range 1 .. 2 := 1;
      Is_Real       : Boolean := True;
      Cache_Index   : Positive range 1 .. Maximum_Rational_Degree := 1;
      Coefficients  : Quadratic_Factor_Coefficients := [others => (Lower => 0.0, Upper => 0.0, Valid => True)];
      Normalization : Interval := (Lower => 1.0, Upper => 1.0, Valid => True);
   end record;

   type Rational_Factor_Array is array (Rational_Degree_Slot) of Rational_Factor;
   function Multiply_Bernstein
     (Left : Certificate_Polynomial; Left_Degree : Natural; Right : Certificate_Polynomial; Right_Degree : Natural)
      return Certificate_Polynomial;
   pragma Annotate (GNATprove, Skip_Proof, Multiply_Bernstein);
   --  Not proved: this recovers from overflow by handling Constraint_Error, or calls what does.
   --  Multiply active controls and pad to the tangent-certificate workspace size.

   function Multiply_Bernstein
     (Left : Certificate_Polynomial; Left_Degree : Natural; Right : Certificate_Polynomial; Right_Degree : Natural)
      return Certificate_Polynomial
   is
      Result : Certificate_Polynomial := [others => Interval_Exact (0.0)];
   begin
      Result (0 .. Left_Degree + Right_Degree) :=
        Multiply_Bernstein (Left (0 .. Left_Degree), Right (0 .. Right_Degree));
      return Result;
   end Multiply_Bernstein;

   function Power_To_Bernstein
     (Power : Quadratic_Factor_Coefficients; Degree : Positive) return Quadratic_Factor_Coefficients
   with Pre => Degree <= 2;
   --  Convert a quadratic-factor polynomial from power to Bernstein coefficients.

   function Power_To_Bernstein
     (Power : Quadratic_Factor_Coefficients; Degree : Positive) return Quadratic_Factor_Coefficients
   is
      Result : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
   begin
      for Bernstein_Index in 0 .. Degree loop
         for Power_Index in 0 .. Bernstein_Index loop
            declare
               Raw_Weight : constant Dimensionless :=
                 Binomial (Bernstein_Index, Power_Index) / Binomial (Degree, Power_Index);
               Weight     : constant Interval := Checked_Interval (Down (Raw_Weight), Up (Raw_Weight));
            begin
               Result (Bernstein_Index) :=
                 Interval_Add (Result (Bernstein_Index), Interval_Multiply (Weight, Power (Power_Index)));
            end;
         end loop;
      end loop;
      return Result;
   end Power_To_Bernstein;

   procedure Add_Scaled_Polynomial
     (Target : in out Certificate_Polynomial; Source : Certificate_Polynomial; Degree : Natural; Scale : Interval)
   with Pre => Degree <= Certificate_Polynomial_Index'Last;
   --  Add a scaled certificate polynomial to Target over its active degree.

   procedure Add_Scaled_Polynomial
     (Target : in out Certificate_Polynomial; Source : Certificate_Polynomial; Degree : Natural; Scale : Interval) is
   begin
      for Index in 0 .. Degree loop
         Target (Index) := Interval_Add (Target (Index), Interval_Multiply (Scale, Source (Index)));
      end loop;
   end Add_Scaled_Polynomial;

   function Certified_Realtime_Tangent_Error
     (Curve                 : Stereographic_Curve;
      Final_Cache           : Rational_Antiderivative;
      Denominator_Scale     : Dimensionless;
      Requested_Point_Error : Length;
      Retained_Certificate  : out Retained_Tangent_Bernstein_Certificate) return Tangent_Certificate_Result
   with Side_Effects;
   pragma Annotate (GNATprove, Skip_Proof, Certified_Realtime_Tangent_Error);
   --  Not proved: this recovers from overflow by handling Constraint_Error, or calls what does.
   --  Certify the real-time tangent approximation and retain its Bernstein certificate.

   function Certified_Realtime_Tangent_Error
     (Curve                 : Stereographic_Curve;
      Final_Cache           : Rational_Antiderivative;
      Denominator_Scale     : Dimensionless;
      Requested_Point_Error : Length;
      Retained_Certificate  : out Retained_Tangent_Bernstein_Certificate) return Tangent_Certificate_Result
   is
      Factors      : Rational_Factor_Array;
      Factor_Count : Natural range 0 .. Maximum_Rational_Degree := 0;

      Cached_Denominator          : Certificate_Polynomial := [others => Interval_Exact (0.0)];
      Cached_Denominator_Degree   : Natural range 0 .. Maximum_Rational_Degree := 0;
      Comparison_Degree           : constant Natural := Maximum_Tangent_Numerator_Degree;
      Compared_Cached_Denominator : Certificate_Polynomial := [others => Interval_Exact (0.0)];
      Cached_Denominator_Scale    : constant Interval := Interval_Exact (Denominator_Scale);
      Minimum_Cached_Denominator  : Dimensionless := 0.0;

      Warp            : constant Interval := Interval_Exact (Curve.Warp_Factor);
      Warp_Difference : constant Interval := Interval_Subtract (Warp, Interval_Exact (1.0));

      --  U = V / H(V), where V is the polynomial chart coordinate and U
      --  is normalized physical distance.
      H : constant array (Natural range 0 .. 1) of Interval := [0 => Warp, 1 => Interval_Negate (Warp_Difference)];

      Ideal_Chart_Squared_Norm       : Certificate_Polynomial := [others => Interval_Exact (0.0)];
      Ideal_Denominator              : Certificate_Polynomial := [others => Interval_Exact (0.0)];
      Ideal_Local_Numerator          : array (Frame_Component_Index) of Certificate_Polynomial :=
        [others => [others => Interval_Exact (0.0)]];
      Ideal_Axis_Numerator           : array (Axis_Name) of Certificate_Polynomial :=
        [others => [others => Interval_Exact (0.0)]];
      Cached_Axis_Numerator          : array (Axis_Name) of Certificate_Polynomial :=
        [others => [others => Interval_Exact (0.0)]];
      Compared_Cached_Axis_Numerator : array (Axis_Name) of Certificate_Polynomial :=
        [others => [others => Interval_Exact (0.0)]];

      procedure Append_Factor
        (Degree        : Positive;
         Is_Real       : Boolean;
         Cache_Index   : Positive;
         Coefficients  : Quadratic_Factor_Coefficients;
         Normalization : Interval);

      procedure Product_Of_Other_Factors
        (Excluded : Positive; Product : out Certificate_Polynomial; Degree : out Natural);

      function Polynomial_Range_Bound
        (Polynomial : Certificate_Polynomial; Degree : Natural; Target : Dimensionless) return Dimensionless;

      procedure Append_Factor
        (Degree        : Positive;
         Is_Real       : Boolean;
         Cache_Index   : Positive;
         Coefficients  : Quadratic_Factor_Coefficients;
         Normalization : Interval) is
      begin
         --  Store each denominator factor normalized to 1 at V = 0. The removed scale is tracked separately so
         --  products retain the authoritative denominator normalization.
         if Degree > 2
           or else Factor_Count = Maximum_Rational_Degree
           or else not Normalization.Valid
           or else (Normalization.Lower <= 0.0 and then Normalization.Upper >= 0.0)
         then
            raise Constraint_Error;
         end if;

         Factor_Count := Factor_Count + 1;
         Factors (Factor_Count) :=
           (Degree        => Degree,
            Is_Real       => Is_Real,
            Cache_Index   => Cache_Index,
            Coefficients  => Coefficients,
            Normalization => Normalization);
      end Append_Factor;

      procedure Product_Of_Other_Factors
        (Excluded : Positive; Product : out Certificate_Polynomial; Degree : out Natural) is
      begin
         --  A partial-fraction numerator for factor qᵢ is multiplied by ∏ⱼ≠ᵢqⱼ before all terms are summed over
         --  the common denominator.
         Product := [others => Interval_Exact (0.0)];
         Product (0) := Cached_Denominator_Scale;
         Degree := 0;

         for Factor_Index in 1 .. Factor_Count loop
            if Factor_Index /= Excluded then
               declare
                  Factor_Polynomial : Certificate_Polynomial := [others => Interval_Exact (0.0)];
               begin
                  for Coefficient_Index in 0 .. Factors (Factor_Index).Degree loop
                     Factor_Polynomial (Coefficient_Index) := Factors (Factor_Index).Coefficients (Coefficient_Index);
                  end loop;
                  Product := Multiply_Bernstein (Product, Degree, Factor_Polynomial, Factors (Factor_Index).Degree);
                  Degree := Degree + Factors (Factor_Index).Degree;
               end;
            end if;
         end loop;
      end Product_Of_Other_Factors;

      function Polynomial_Range_Bound
        (Polynomial : Certificate_Polynomial; Degree : Natural; Target : Dimensionless) return Dimensionless
      is
         Maximum_Subdivision_Depth : constant := 6;
         Centres                   : Certificate_Centres := [others => 0.0];
         Radii                     : Certificate_Centres := [others => 0.0];

         function Coefficient_Bound (Local_Centres, Local_Radii : Certificate_Centres) return Dimensionless;

         function Refined_Bound
           (Local_Centres, Local_Radii : Certificate_Centres; Depth : Natural) return Dimensionless
         with Subprogram_Variant => (Increases => Depth);

         function Coefficient_Bound (Local_Centres, Local_Radii : Certificate_Centres) return Dimensionless is
            Bound : Dimensionless := 0.0;
         begin
            --  The value of a Bernstein polynomial lies in the convex hull of its controls.
            for Index in 0 .. Degree loop
               Bound := Dimensionless'Max (Bound, abs Local_Centres (Index) + Local_Radii (Index));
            end loop;
            return Up (Bound);
         end Coefficient_Bound;

         function Refined_Bound
           (Local_Centres, Local_Radii : Certificate_Centres; Depth : Natural) return Dimensionless
         is
            Current                    : constant Dimensionless := Coefficient_Bound (Local_Centres, Local_Radii);
            Work_Centres               : Certificate_Centres := Local_Centres;
            Work_Radii                 : Certificate_Centres := Local_Radii;
            Left_Centres, Left_Radii   : Certificate_Centres := [others => 0.0];
            Right_Centres, Right_Radii : Certificate_Centres := [others => 0.0];
         begin
            if Current <= Target or else Depth = Maximum_Subdivision_Depth or else Degree = 0 then
               return Current;
            end if;

            Left_Centres (0) := Work_Centres (0);
            Left_Radii (0) := Work_Radii (0);
            Right_Centres (Degree) := Work_Centres (Degree);
            Right_Radii (Degree) := Work_Radii (Degree);

            --  De Casteljau subdivision at one half preserves Bernstein
            --  form. The radius tracks roundoff introduced while forming
            --  every midpoint.
            for Level in 1 .. Degree loop
               for Index in 0 .. Degree - Level loop
                  declare
                     New_Centre : constant Dimensionless := 0.5 * (Work_Centres (Index) + Work_Centres (Index + 1));
                     Magnitude  : constant Dimensionless :=
                       abs Work_Centres (Index)
                       + abs Work_Centres (Index + 1)
                       + Work_Radii (Index)
                       + Work_Radii (Index + 1)
                       + Dimensionless'Model_Small;
                  begin
                     Work_Centres (Index) := New_Centre;
                     Work_Radii (Index) :=
                       (0.5 * (Work_Radii (Index) + Work_Radii (Index + 1))
                        + 8.0 * Dimensionless'Model_Epsilon * Magnitude)
                       * (1.0 + 16.0 * Dimensionless'Model_Epsilon);
                  end;
               end loop;
               Left_Centres (Level) := Work_Centres (0);
               Left_Radii (Level) := Work_Radii (0);
               Right_Centres (Degree - Level) := Work_Centres (Degree - Level);
               Right_Radii (Degree - Level) := Work_Radii (Degree - Level);
            end loop;

            return
              Dimensionless'Max
                (Refined_Bound (Left_Centres, Left_Radii, Depth + 1),
                 Refined_Bound (Right_Centres, Right_Radii, Depth + 1));
         end Refined_Bound;
      begin
         for Index in 0 .. Degree loop
            if not Polynomial (Index).Valid then
               return Dimensionless'Last;
            end if;

            Centres (Index) := Polynomial (Index).Lower + 0.5 * (Polynomial (Index).Upper - Polynomial (Index).Lower);
            Radii (Index) :=
              Dimensionless'Max
                (abs (Centres (Index) - Polynomial (Index).Lower), abs (Polynomial (Index).Upper - Centres (Index)))
              * (1.0 + 8.0 * Dimensionless'Model_Epsilon);
         end loop;
         return Refined_Bound (Centres, Radii, 0);
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Polynomial_Range_Bound;

   begin
      Retained_Certificate := (others => <>);
      if not Rational_Antiderivative_Is_Well_Formed (Final_Cache)
        or else Denominator_Scale <= 0.0
        or else not Is_Finite (Denominator_Scale)
        or else Requested_Point_Error <= 0.0 * mm
        or else Curve.Evaluator_Data.Length_Value <= 0.0 * mm
      then
         return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
      end if;

      --  Reconstruct every factor of the final U-space cache after pulling it back through U = V/H(V).
      --  Normalizing each factor at V = 0 avoids large and cancellation-prone power coefficients.
      for Index in 1 .. Final_Cache.Real_Pole_Count loop
         declare
            Slot             : Rational_Pole_Slot renames Final_Cache.Pole_Slots (Index);
            Pole             : constant Interval := Interval_Exact (Slot.Pole_Component);
            Factor_Power     : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
            Normalized_Power : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
            Normalization    : Interval;
         begin
            Factor_Power (0) := Interval_Negate (Interval_Multiply (Pole, H (0)));
            Factor_Power (1) := Interval_Add (Interval_Exact (1.0), Interval_Multiply (Pole, Warp_Difference));
            Normalization := Factor_Power (0);
            Normalized_Power (0) := Interval_Exact (1.0);
            Normalized_Power (1) := Interval_Divide (Factor_Power (1), Normalization);
            Append_Factor
              (Degree        => 1,
               Is_Real       => True,
               Cache_Index   => Index,
               Coefficients  => Power_To_Bernstein (Normalized_Power, 1),
               Normalization => Normalization);
         end;
      end loop;

      for Pair in 1 .. Final_Cache.Pair_Count loop
         declare
            Pair_Index       : constant Rational_Pair_Index := Rational_Pair_Index (Pair);
            Real_Slot        : Rational_Pole_Slot renames
              Final_Cache.Pole_Slots (Complex_Pair_Real_Slot (Final_Cache, Pair_Index));
            Imaginary_Slot   : Rational_Pole_Slot renames
              Final_Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Final_Cache, Pair_Index));
            Pole_Real        : constant Interval := Interval_Exact (Real_Slot.Pole_Component);
            Pole_Imaginary   : constant Interval := Interval_Exact (Imaginary_Slot.Pole_Component);
            Real_Linear      : constant array (Natural range 0 .. 1) of Interval :=
              [0 => Interval_Negate (Interval_Multiply (Pole_Real, H (0))),
               1 => Interval_Subtract (Interval_Exact (1.0), Interval_Multiply (Pole_Real, H (1)))];
            Imaginary_Linear : constant array (Natural range 0 .. 1) of Interval :=
              [for Index in 0 .. 1 => Interval_Multiply (Pole_Imaginary, H (Index))];
            Factor_Power     : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
            Normalized_Power : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
            Normalization    : Interval;
         begin
            for Left_Index in 0 .. 1 loop
               for Right_Index in 0 .. 1 loop
                  Factor_Power (Left_Index + Right_Index) :=
                    Interval_Add
                      (Factor_Power (Left_Index + Right_Index),
                       Interval_Add
                         (Interval_Multiply (Real_Linear (Left_Index), Real_Linear (Right_Index)),
                          Interval_Multiply (Imaginary_Linear (Left_Index), Imaginary_Linear (Right_Index))));
               end loop;
            end loop;
            Normalization := Factor_Power (0);
            Normalized_Power (0) := Interval_Exact (1.0);
            for Index in 1 .. 2 loop
               Normalized_Power (Index) := Interval_Divide (Factor_Power (Index), Normalization);
            end loop;
            Append_Factor
              (Degree        => 2,
               Is_Real       => False,
               Cache_Index   => Pair,
               Coefficients  => Power_To_Bernstein (Normalized_Power, 2),
               Normalization => Normalization);
         end;
      end loop;

      Cached_Denominator (0) := Cached_Denominator_Scale;
      --  Multiply the normalized linear and quadratic factors to reconstruct the cached denominator Q(V).
      for Factor_Index in 1 .. Factor_Count loop
         declare
            Factor_Polynomial : Certificate_Polynomial := [others => Interval_Exact (0.0)];
         begin
            for Coefficient_Index in 0 .. Factors (Factor_Index).Degree loop
               Factor_Polynomial (Coefficient_Index) := Factors (Factor_Index).Coefficients (Coefficient_Index);
            end loop;
            Cached_Denominator :=
              Multiply_Bernstein
                (Cached_Denominator, Cached_Denominator_Degree, Factor_Polynomial, Factors (Factor_Index).Degree);
            Cached_Denominator_Degree := Cached_Denominator_Degree + Factors (Factor_Index).Degree;
         end;
      end loop;

      for Axis in Axis_Name loop
         Add_Scaled_Polynomial
           (Cached_Axis_Numerator (Axis),
            Cached_Denominator,
            Cached_Denominator_Degree,
            Interval_Exact (Final_Cache.Constant_Tangent (Axis)));
      end loop;

      --  Add each partial-fraction numerator after multiplying by every denominator factor except its own.
      for Factor_Index in 1 .. Factor_Count loop
         declare
            Other_Factors : Certificate_Polynomial;
            Other_Degree  : Natural;
         begin
            Product_Of_Other_Factors (Factor_Index, Other_Factors, Other_Degree);

            if Factors (Factor_Index).Is_Real then
               declare
                  Slot_Index : constant Rational_Degree_Slot := Factors (Factor_Index).Cache_Index;
                  Slot       : Rational_Pole_Slot renames Final_Cache.Pole_Slots (Slot_Index);
               begin
                  for Axis in Axis_Name loop
                     declare
                        Residue             : constant Interval := Interval_Exact (Slot.Residue_Component (Axis));
                        Numerator_Power     : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
                        Numerator_Bernstein : Quadratic_Factor_Coefficients;
                        Numerator           : Certificate_Polynomial := [others => Interval_Exact (0.0)];
                        Term                : Certificate_Polynomial;
                     begin
                        for Index in 0 .. 1 loop
                           Numerator_Power (Index) :=
                             Interval_Divide
                               (Interval_Multiply (Residue, H (Index)), Factors (Factor_Index).Normalization);
                        end loop;
                        Numerator_Bernstein := Power_To_Bernstein (Numerator_Power, 1);
                        for Index in 0 .. 1 loop
                           Numerator (Index) := Numerator_Bernstein (Index);
                        end loop;
                        Term := Multiply_Bernstein (Other_Factors, Other_Degree, Numerator, 1);
                        Add_Scaled_Polynomial
                          (Cached_Axis_Numerator (Axis), Term, Other_Degree + 1, Interval_Exact (1.0));
                     end;
                  end loop;
               end;
            else
               declare
                  Pair           : constant Rational_Pair_Index :=
                    Rational_Pair_Index (Factors (Factor_Index).Cache_Index);
                  Real_Slot      : Rational_Pole_Slot renames
                    Final_Cache.Pole_Slots (Complex_Pair_Real_Slot (Final_Cache, Pair));
                  Imaginary_Slot : Rational_Pole_Slot renames
                    Final_Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Final_Cache, Pair));
                  Pole_Real      : constant Interval := Interval_Exact (Real_Slot.Pole_Component);
                  Pole_Imaginary : constant Interval := Interval_Exact (Imaginary_Slot.Pole_Component);
               begin
                  for Axis in Axis_Name loop
                     declare
                        Residue_Real        : constant Interval := Interval_Exact (Real_Slot.Residue_Component (Axis));
                        Residue_Imaginary   : constant Interval :=
                          Interval_Exact (Imaginary_Slot.Residue_Component (Axis));
                        Real_Linear         : constant array (Natural range 0 .. 1) of Interval :=
                          [0 => Interval_Negate (Interval_Multiply (Pole_Real, H (0))),
                           1 => Interval_Subtract (Interval_Exact (1.0), Interval_Multiply (Pole_Real, H (1)))];
                        Inner               : array (Natural range 0 .. 1) of Interval;
                        Numerator_Power     : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
                        Numerator_Bernstein : Quadratic_Factor_Coefficients;
                        Pair_Numerator      : Certificate_Polynomial := [others => Interval_Exact (0.0)];
                        Term                : Certificate_Polynomial;
                     begin
                        for Index in 0 .. 1 loop
                           Inner (Index) :=
                             Interval_Subtract
                               (Interval_Multiply (Residue_Real, Real_Linear (Index)),
                                Interval_Multiply (Residue_Imaginary, Interval_Multiply (Pole_Imaginary, H (Index))));
                        end loop;

                        for H_Index in 0 .. 1 loop
                           for Inner_Index in 0 .. 1 loop
                              Numerator_Power (H_Index + Inner_Index) :=
                                Interval_Add
                                  (Numerator_Power (H_Index + Inner_Index),
                                   Interval_Multiply
                                     (Interval_Exact (2.0), Interval_Multiply (H (H_Index), Inner (Inner_Index))));
                           end loop;
                        end loop;

                        for Index in 0 .. 2 loop
                           Numerator_Power (Index) :=
                             Interval_Divide (Numerator_Power (Index), Factors (Factor_Index).Normalization);
                        end loop;
                        Numerator_Bernstein := Power_To_Bernstein (Numerator_Power, 2);
                        for Index in 0 .. 2 loop
                           Pair_Numerator (Index) := Numerator_Bernstein (Index);
                        end loop;
                        Term := Multiply_Bernstein (Other_Factors, Other_Degree, Pair_Numerator, 2);
                        Add_Scaled_Polynomial
                          (Cached_Axis_Numerator (Axis), Term, Other_Degree + 2, Interval_Exact (1.0));
                     end;
                  end loop;
               end;
            end if;
         end;
      end loop;

      --  Build the authoritative ideal tangent directly from the stored degree-eight Bernstein chart:
      --
      --     local numerator = (1 - Y·Y, 2Y)
      --     denominator      = 1 + Y·Y.
      --
      --  Avoiding a power-basis round trip is important: sharp curves have large intermediate power coefficients
      --  whose cancellation would make the interval certificate needlessly wide.
      for Component in Chart_Component_Index loop
         declare
            Chart  : Certificate_Polynomial := [others => Interval_Exact (0.0)];
            Square : Certificate_Polynomial;
         begin
            for Index in Chart_Coefficient_Index loop
               Chart (Index) := Interval_Exact (Curve.Coefficients (Index, Component));
            end loop;
            Square := Multiply_Bernstein (Chart, Fixed_Chart_Degree, Chart, Fixed_Chart_Degree);
            for Index in 0 .. Comparison_Degree loop
               Ideal_Chart_Squared_Norm (Index) := Interval_Add (Ideal_Chart_Squared_Norm (Index), Square (Index));
            end loop;
         end;
      end loop;

      Ideal_Denominator := Ideal_Chart_Squared_Norm;
      for Index in 0 .. Comparison_Degree loop
         Ideal_Denominator (Index) := Interval_Add (Ideal_Denominator (Index), Interval_Exact (1.0));
         Ideal_Local_Numerator (0) (Index) :=
           Interval_Subtract (Interval_Exact (1.0), Ideal_Chart_Squared_Norm (Index));
      end loop;

      for Component in Chart_Component_Index loop
         declare
            Chart    : Certificate_Polynomial := [others => Interval_Exact (0.0)];
            Elevated : Certificate_Polynomial;
         begin
            for Index in Chart_Coefficient_Index loop
               Chart (Index) := Interval_Exact (Curve.Coefficients (Index, Component));
            end loop;
            Elevated := Elevate_Bernstein (Chart, Fixed_Chart_Degree, Comparison_Degree);
            for Index in 0 .. Comparison_Degree loop
               Ideal_Local_Numerator (Component) (Index) := Interval_Multiply (Interval_Exact (2.0), Elevated (Index));
            end loop;
         end;
      end loop;

      for Axis in Axis_Name loop
         for Component in Frame_Component_Index loop
            Add_Scaled_Polynomial
              (Ideal_Axis_Numerator (Axis),
               Ideal_Local_Numerator (Component),
               Comparison_Degree,
               Interval_Exact (Curve.Frame (Component) (Axis)));
         end loop;
      end loop;

      Compared_Cached_Denominator :=
        Elevate_Bernstein (Cached_Denominator, Cached_Denominator_Degree, Comparison_Degree);
      for Axis in Axis_Name loop
         Compared_Cached_Axis_Numerator (Axis) :=
           Elevate_Bernstein (Cached_Axis_Numerator (Axis), Cached_Denominator_Degree, Comparison_Degree);
      end loop;

      --  D is at least one because D = 1 + Y·Y. If the reconstructed cached Q differs from D by less than one,
      --  then
      --
      --     Q ≥ D - |Q - D| ≥ 1 - |Q - D| > 0,
      --
      --  so division by Q is safe over the complete curve.
      declare
         Denominator_Difference : Certificate_Polynomial := [others => Interval_Exact (0.0)];
         Difference_Bound       : Dimensionless;
      begin
         for Index in 0 .. Comparison_Degree loop
            Denominator_Difference (Index) :=
              Interval_Subtract (Compared_Cached_Denominator (Index), Ideal_Denominator (Index));
         end loop;
         Difference_Bound := Polynomial_Range_Bound (Denominator_Difference, Comparison_Degree, 0.25);
         if Difference_Bound = Dimensionless'Last or else not Is_Finite (Difference_Bound) then
            return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
         elsif Difference_Bound >= 1.0 then
            return (Status => Tangent_Could_Not_Be_Certified);
         end if;

         Minimum_Cached_Denominator := Down (1.0 - Difference_Bound);
         if Minimum_Cached_Denominator <= 0.0 then
            return (Status => Tangent_Could_Not_Be_Certified);
         end if;
      end;

      --  Publish the same outward-rounded common-denominator form used by the ideal-to-realtime tangent proof.
      --  Reusing these controls for later derivative bounds avoids reconstructing the rational function from poles
      --  a second time, and setting Valid only after Q has a positive lower bound prevents a partial certificate
      --  from escaping on an earlier failure path.
      Retained_Certificate.Valid := True;
      Retained_Certificate.Degree := Cached_Denominator_Degree;
      Retained_Certificate.Minimum_Denominator := Minimum_Cached_Denominator;
      for Index in 0 .. Cached_Denominator_Degree loop
         Retained_Certificate.Denominator (Index) := Cached_Denominator (Index);
         for Axis in Axis_Name loop
            Retained_Certificate.Axis_Numerators (Axis) (Index) := Cached_Axis_Numerator (Axis) (Index);
         end loop;
      end loop;

      declare
         Squared_Error_Sum          : Dimensionless := 0.0;
         Axis_Count                 : constant Dimensionless :=
           Dimensionless (Axis_Name'Pos (Axis_Name'Last) - Axis_Name'Pos (Axis_Name'First) + 1);
         Requested_Normalized_Error : constant Dimensionless :=
           Requested_Point_Error / Curve.Evaluator_Data.Length_Value;
         Subdivision_Target         : constant Dimensionless :=
           Minimum_Cached_Denominator * Requested_Normalized_Error / (16.0 * Dimensionless_Math.Sqrt (Axis_Count));
      begin
         for Axis in Axis_Name loop
            declare
               --  Compare cached N/Q with ideal P/D without interval division:
               --
               --     N/Q - P/D = (N·D - P·Q)/(Q·D).
               --
               --  Since D ≥ 1 and Q ≥ Minimum_Cached_Denominator, the axis error is bounded by the numerator
               --  residual divided by Minimum_Cached_Denominator.
               Cached_Times_Ideal_Denominator : constant Certificate_Polynomial :=
                 Multiply_Bernstein
                   (Compared_Cached_Axis_Numerator (Axis), Comparison_Degree, Ideal_Denominator, Comparison_Degree);
               Ideal_Times_Cached_Denominator : constant Certificate_Polynomial :=
                 Multiply_Bernstein
                   (Ideal_Axis_Numerator (Axis), Comparison_Degree, Compared_Cached_Denominator, Comparison_Degree);
               Cross_Residual                 : Certificate_Polynomial := [others => Interval_Exact (0.0)];
               Residual_Bound                 : Dimensionless;
               Axis_Error                     : Dimensionless;
            begin
               for Index in 0 .. 2 * Comparison_Degree loop
                  Cross_Residual (Index) :=
                    Interval_Subtract (Cached_Times_Ideal_Denominator (Index), Ideal_Times_Cached_Denominator (Index));
               end loop;
               Residual_Bound := Polynomial_Range_Bound (Cross_Residual, 2 * Comparison_Degree, Subdivision_Target);
               if Residual_Bound = Dimensionless'Last or else not Is_Finite (Residual_Bound) then
                  return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
               end if;
               Axis_Error := Up (Residual_Bound / Minimum_Cached_Denominator);
               Squared_Error_Sum := Up (Squared_Error_Sum + Up (Axis_Error * Axis_Error));
            end;
         end loop;
         declare
            Error_Bound : constant Dimensionless := Certified_Upper_Square_Root (Squared_Error_Sum);
         begin
            --  Combine independent axis bounds in Euclidean norm. Build_Rational_Representation later multiplies
            --  this unit-tangent error by curve length to bound integrated position error.
            if Error_Bound = Dimensionless'Last or else not Is_Finite (Error_Bound) then
               return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
            else
               return (Status => Tangent_Was_Certified, Error_Bound => Error_Bound);
            end if;
         end;
      end;
   exception
      when Constraint_Error =>
         return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
   end Certified_Realtime_Tangent_Error;

   function Retain_Bernstein_Tangent_Certificate (Curve : in out Stereographic_Curve) return Boolean is
      Start_Square : Dimensionless := 0.0;
      Certificate  : Retained_Tangent_Bernstein_Certificate;
   begin
      --  The first Bernstein control is the chart's value at the start, where the ideal denominator is 1 + Y·Y.
      for Component in Chart_Component_Index loop
         pragma Loop_Invariant (Start_Square >= 0.0 and then Start_Square <= 4.0 * Tame_Limit * Tame_Limit);
         --  The chart of a constructed curve is within this limit. Beyond it there is no reconstruction.
         if not (abs Curve.Coefficients (0, Component) <= Tame_Limit)
           or else not (Start_Square <= Tame_Limit * Tame_Limit)
         then
            return False;
         end if;
         Start_Square := Start_Square + Curve.Coefficients (0, Component) * Curve.Coefficients (0, Component);
      end loop;
      declare
         Result : Tangent_Certificate_Result;
         pragma Unreferenced (Result);
         --  Only the reconstruction is wanted. It becomes valid once its denominator is proven positive, which is
         --  all the derivative bounds rely on, so a failure of the tangent error bound computed afterwards does not
         --  discard it.
      begin
         Result :=
           Certified_Realtime_Tangent_Error
             (Curve,
              Curve.Evaluator_Data.Antiderivative_Cache,
              Start_Square + 1.0,
              Length'Max (Curve.Certified_Position_Error, Length'Model_Small),
              Certificate);
         if Certificate.Valid then
            Curve.Retained_Tangent_Certificate := Certificate;
            return True;
         else
            return False;
         end if;
      end;
   end Retain_Bernstein_Tangent_Certificate;

   Root_Coefficient_Limit : constant := 1.0E100;
   --  Largest coefficient of a polynomial whose roots Iterate_Conjugate_Roots iterates.

   Root_Leading_Floor : constant := 2.0**(-300);
   --  Smallest magnitude of its leading coefficient.

   Root_Limit : constant := 1.0E6;
   --  Largest real or imaginary part of an accepted iterate, and of the centre and radius of the starting circle.

   Root_Iterate_Limit : constant := 2.0**21;
   --  Largest real or imaginary part of a stored iterate: a starting point is within the centre plus the radius.

   Root_Value_Limit : constant := 2.0**300;
   --  Largest part of the polynomial's value or of its derivative from which an Aberth correction is formed.

   Root_Gap_Floor : constant := 2.0**(-300);
   --  Smallest imaginary part of an iterate, squared distance between two iterates, and squared modulus of the
   --  Aberth denominator, which a correction divides by.

   Root_Slope_Floor : constant := 2.0**(-400);
   --  Smallest squared modulus of the derivative which a Newton correction divides by.

   Newton_Limit : constant := 2.0**150;
   --  Largest part of a Newton correction which the Aberth correction is formed from.

   Root_Term_Limit : constant := 2.0**323;
   Root_Sum_Limit  : constant := 2.0**327;
   --  Largest term, and largest sum, of the reciprocal distances to the other roots.

   Root_Correction_Limit : constant := 2.0**930;
   --  Largest part of an Aberth correction.

   Horner_Result_Limit : constant := 2.0**717;
   --  Largest value of a Horner evaluation of sixteen steps.

   function Is_Tame (Coefficients : Root_Polynomial; Degree : Natural) return Boolean
   is (Degree <= Maximum_Rational_Degree
       and then (for all Index in 0 .. Degree => abs Coefficients (Index) <= Root_Coefficient_Limit));
   --  Whether the coefficients up to Degree are within the limit.

   function Are_Bounded (Real_Parts, Imaginary_Parts : Conjugate_Root_Parts) return Boolean
   is (for all K in Conjugate_Root_Index =>
         abs Real_Parts (K) <= Root_Iterate_Limit and then abs Imaginary_Parts (K) <= Root_Iterate_Limit);
   --  Whether every iterate is within the limit.

   function Horner_Limit (Steps : Natural) return Dimensionless
   is (case Steps is
         when 0      => 2.0**333,
         when 1      => 2.0**357,
         when 2      => 2.0**381,
         when 3      => 2.0**405,
         when 4      => 2.0**429,
         when 5      => 2.0**453,
         when 6      => 2.0**477,
         when 7      => 2.0**501,
         when 8      => 2.0**525,
         when 9      => 2.0**549,
         when 10     => 2.0**573,
         when 11     => 2.0**597,
         when 12     => 2.0**621,
         when 13     => 2.0**645,
         when 14     => 2.0**669,
         when 15     => 2.0**693,
         when others => Horner_Result_Limit)
   with Ghost, Pre => Steps <= Maximum_Rational_Degree;
   --  A bound on a Horner evaluation after so many steps: each step multiplies by at most 2²³ and adds at most
   --  the bound again.

   procedure Lemma_Horner_Limit_Grows (Steps : Natural)
   with
     Ghost,
     Pre  => Steps < Maximum_Rational_Degree,
     Post =>
       Horner_Limit (Steps) >= 2.0**333
       and then Horner_Limit (Steps) <= 2.0**693
       and then Horner_Limit (Steps) * 2.0**24 <= Horner_Limit (Steps + 1)
       and then Horner_Limit (Steps + 1) <= Horner_Result_Limit;

   procedure Lemma_Horner_Limit_Grows (Steps : Natural) is null;

   procedure Lemma_Horner_Sum (Pair, Add, Limit : Dimensionless)
   with
     Ghost,
     Pre  =>
       Limit >= 2.0**333 and then Limit <= 2.0**693 and then abs Pair <= Limit * 2.0**23 and then abs Add <= Limit,
     Post => abs (Pair + Add) <= Limit * 2.0**24;

   procedure Lemma_Horner_Sum (Pair, Add, Limit : Dimensionless) is null;

   procedure Lemma_Horner_Difference (A_Re, A_Im, X, Y, Limit : Dimensionless)
   with
     Ghost,
     Pre  =>
       Limit >= 2.0**333
       and then Limit <= 2.0**693
       and then abs A_Re <= Limit
       and then abs A_Im <= Limit
       and then abs X <= Root_Iterate_Limit
       and then abs Y <= Root_Iterate_Limit,
     Post => abs (A_Re * X - A_Im * Y) <= Limit * 2.0**23;

   procedure Lemma_Horner_Difference (A_Re, A_Im, X, Y, Limit : Dimensionless) is null;

   procedure Lemma_Horner_Pair (A_Re, A_Im, X, Y, Limit : Dimensionless)
   with
     Ghost,
     Pre  =>
       Limit >= 2.0**333
       and then Limit <= 2.0**693
       and then abs A_Re <= Limit
       and then abs A_Im <= Limit
       and then abs X <= Root_Iterate_Limit
       and then abs Y <= Root_Iterate_Limit,
     Post => abs (A_Re * Y + A_Im * X) <= Limit * 2.0**23;

   procedure Lemma_Horner_Pair (A_Re, A_Im, X, Y, Limit : Dimensionless) is null;

   procedure Lemma_Horner_Real (A_Re, A_Im, X, Y, Add, Limit : Dimensionless)
   with
     Ghost,
     Pre  =>
       Limit >= 2.0**333
       and then Limit <= 2.0**693
       and then abs A_Re <= Limit
       and then abs A_Im <= Limit
       and then abs Add <= Limit
       and then abs X <= Root_Iterate_Limit
       and then abs Y <= Root_Iterate_Limit,
     Post => abs (A_Re * X - A_Im * Y + Add) <= Limit * 2.0**24;

   procedure Lemma_Horner_Real (A_Re, A_Im, X, Y, Add, Limit : Dimensionless) is
   begin
      Lemma_Horner_Difference (A_Re, A_Im, X, Y, Limit);
      Lemma_Horner_Sum (A_Re * X - A_Im * Y, Add, Limit);
   end Lemma_Horner_Real;

   procedure Lemma_Horner_Imaginary (A_Re, A_Im, X, Y, Add, Limit : Dimensionless)
   with
     Ghost,
     Pre  =>
       Limit >= 2.0**333
       and then Limit <= 2.0**693
       and then abs A_Re <= Limit
       and then abs A_Im <= Limit
       and then abs Add <= Limit
       and then abs X <= Root_Iterate_Limit
       and then abs Y <= Root_Iterate_Limit,
     Post => abs (A_Re * Y + A_Im * X + Add) <= Limit * 2.0**24;

   procedure Lemma_Horner_Imaginary (A_Re, A_Im, X, Y, Add, Limit : Dimensionless) is
   begin
      Lemma_Horner_Pair (A_Re, A_Im, X, Y, Limit);
      Lemma_Horner_Sum (A_Re * Y + A_Im * X, Add, Limit);
   end Lemma_Horner_Imaginary;

   procedure Lemma_Horner_Imaginary_Plain (A_Re, A_Im, X, Y, Limit : Dimensionless)
   with
     Ghost,
     Pre  =>
       Limit >= 2.0**333
       and then Limit <= 2.0**693
       and then abs A_Re <= Limit
       and then abs A_Im <= Limit
       and then abs X <= Root_Iterate_Limit
       and then abs Y <= Root_Iterate_Limit,
     Post => abs (A_Re * Y + A_Im * X) <= Limit * 2.0**24;

   procedure Lemma_Horner_Imaginary_Plain (A_Re, A_Im, X, Y, Limit : Dimensionless) is null;

   procedure Lemma_Horner_Size (Size, Modulus, Coefficient, Limit : Dimensionless)
   with
     Ghost,
     Pre  =>
       Limit >= 2.0**333
       and then Limit <= 2.0**693
       and then Size >= 0.0
       and then Size <= Limit
       and then Modulus >= 0.0
       and then Modulus <= 2.0 * Root_Iterate_Limit
       and then abs Coefficient <= Root_Coefficient_Limit,
     Post => Size * Modulus + abs Coefficient >= 0.0 and then Size * Modulus + abs Coefficient <= Limit * 2.0**24;

   procedure Lemma_Horner_Size (Size, Modulus, Coefficient, Limit : Dimensionless) is null;

   procedure Lemma_Horner_Plain (Value, Centre, Add, Limit : Dimensionless)
   with
     Ghost,
     Pre  =>
       Limit >= 2.0**333
       and then Limit <= 2.0**693
       and then abs Value <= Limit
       and then abs Centre <= Root_Limit
       and then abs Add <= Root_Coefficient_Limit,
     Post => abs (Value * Centre + Add) <= Limit * 2.0**24;

   procedure Lemma_Horner_Plain (Value, Centre, Add, Limit : Dimensionless) is null;

   procedure Evaluate_Root_Polynomial
     (Coefficients                 : Root_Polynomial;
      Degree                       : Natural;
      X, Y, Modulus                : Dimensionless;
      P_Re, P_Im, D_Re, D_Im, Size : out Dimensionless)
   with
     Pre  =>
       Degree in 1 .. Maximum_Rational_Degree
       and then Is_Tame (Coefficients, Degree)
       and then abs X <= Root_Iterate_Limit
       and then abs Y <= Root_Iterate_Limit
       and then Modulus >= 0.0
       and then Modulus <= 2.0 * Root_Iterate_Limit,
     Post =>
       abs P_Re <= Horner_Result_Limit
       and then abs P_Im <= Horner_Result_Limit
       and then abs D_Re <= Horner_Result_Limit
       and then abs D_Im <= Horner_Result_Limit
       and then Size >= 0.0
       and then Size <= Horner_Result_Limit;
   pragma Inline (Evaluate_Root_Polynomial);
   --  Horner evaluation at X + iY of the polynomial, of its derivative and of the magnitude bound Σ |aᵢ|·|z|ⁱ.

   procedure Evaluate_Root_Polynomial
     (Coefficients                 : Root_Polynomial;
      Degree                       : Natural;
      X, Y, Modulus                : Dimensionless;
      P_Re, P_Im, D_Re, D_Im, Size : out Dimensionless)
   is
      pragma Suppress (Overflow_Check);
      Next : Dimensionless;
   begin
      pragma Assert (abs Coefficients (Degree) <= Root_Coefficient_Limit);
      P_Re := Coefficients (Degree);
      P_Im := 0.0;
      D_Re := 0.0;
      D_Im := 0.0;
      Size := abs Coefficients (Degree);
      for Index in reverse 0 .. Degree - 1 loop
         pragma
           Loop_Invariant
             (abs P_Re <= Horner_Limit (Degree - 1 - Index)
              and then abs P_Im <= Horner_Limit (Degree - 1 - Index)
              and then abs D_Re <= Horner_Limit (Degree - 1 - Index)
              and then abs D_Im <= Horner_Limit (Degree - 1 - Index)
              and then Size >= 0.0
              and then Size <= Horner_Limit (Degree - 1 - Index));
         Lemma_Horner_Limit_Grows (Degree - 1 - Index);
         pragma Assert (abs Coefficients (Index) <= Root_Coefficient_Limit);
         Lemma_Horner_Real (D_Re, D_Im, X, Y, P_Re, Horner_Limit (Degree - 1 - Index));
         Lemma_Horner_Imaginary (D_Re, D_Im, X, Y, P_Im, Horner_Limit (Degree - 1 - Index));
         Lemma_Horner_Real (P_Re, P_Im, X, Y, Coefficients (Index), Horner_Limit (Degree - 1 - Index));
         Lemma_Horner_Imaginary_Plain (P_Re, P_Im, X, Y, Horner_Limit (Degree - 1 - Index));
         Lemma_Horner_Size (Size, Modulus, Coefficients (Index), Horner_Limit (Degree - 1 - Index));
         Next := D_Re * X - D_Im * Y + P_Re;
         D_Im := D_Re * Y + D_Im * X + P_Im;
         D_Re := Next;
         Next := P_Re * X - P_Im * Y + Coefficients (Index);
         P_Im := P_Re * Y + P_Im * X;
         P_Re := Next;
         Size := Size * Modulus + abs Coefficients (Index);
      end loop;
   end Evaluate_Root_Polynomial;

   procedure Lemma_Root_Sum_Grows (Sum, Term : Dimensionless; Count : Positive)
   with
     Ghost,
     Pre  =>
       Count <= Conjugate_Root_Index'Last
       and then abs Sum <= Dimensionless (Count) * Root_Term_Limit
       and then abs Term <= Root_Term_Limit,
     Post =>
       abs (Sum + Term) <= Dimensionless (Count + 1) * Root_Term_Limit
       and then abs (Sum - Term) <= Dimensionless (Count + 1) * Root_Term_Limit;

   procedure Lemma_Root_Sum_Grows (Sum, Term : Dimensionless; Count : Positive) is null;

   procedure Lemma_Root_Sum_Stays (Sum : Dimensionless; Count : Positive)
   with
     Ghost,
     Pre  => Count <= Conjugate_Root_Index'Last + 1 and then abs Sum <= Dimensionless (Count) * Root_Term_Limit,
     Post => abs Sum <= Dimensionless (Count + 1) * Root_Term_Limit and then abs Sum <= Root_Sum_Limit;

   procedure Lemma_Root_Sum_Stays (Sum : Dimensionless; Count : Positive) is null;

   procedure Sum_Over_Other_Roots
     (Real_Parts, Imaginary_Parts : Conjugate_Root_Parts;
      Half                        : Natural;
      K                           : Conjugate_Root_Index;
      X, Y                        : Dimensionless;
      S_Re, S_Im                  : out Dimensionless;
      Separated                   : out Boolean)
   with
     Pre  =>
       Half <= Conjugate_Root_Index'Last
       and then Are_Bounded (Real_Parts, Imaginary_Parts)
       and then abs X <= Root_Iterate_Limit
       and then Y >= Root_Gap_Floor
       and then Y <= Root_Iterate_Limit,
     Post => abs S_Re <= Root_Sum_Limit and then abs S_Im <= Root_Sum_Limit;
   pragma Inline (Sum_Over_Other_Roots);
   --  S = Σ 1/(z - zⱼ) at z = X + iY over every root other than root K, starting with that root's own conjugate.
   --  Separated is False, and the sums zero, if z is within the gap floor of another root or of its conjugate.

   procedure Sum_Over_Other_Roots
     (Real_Parts, Imaginary_Parts : Conjugate_Root_Parts;
      Half                        : Natural;
      K                           : Conjugate_Root_Index;
      X, Y                        : Dimensionless;
      S_Re, S_Im                  : out Dimensionless;
      Separated                   : out Boolean)
   is
      pragma Suppress (Overflow_Check);
   begin
      S_Re := 0.0;
      S_Im := -0.5 / Y;
      Separated := True;
      pragma Assert (abs S_Im <= Root_Term_Limit);
      for J in 1 .. Half loop
         pragma
           Loop_Invariant
             (abs S_Re <= Dimensionless (J) * Root_Term_Limit
              and then abs S_Im <= Dimensionless (J) * Root_Term_Limit);
         if J /= K then
            pragma
              Assert (abs Real_Parts (J) <= Root_Iterate_Limit and then abs Imaginary_Parts (J) <= Root_Iterate_Limit);
            declare
               DX        : constant Dimensionless := X - Real_Parts (J);
               Upper_Y   : constant Dimensionless := Y - Imaginary_Parts (J);
               Lower_Y   : constant Dimensionless := Y + Imaginary_Parts (J);
               Upper_Gap : constant Dimensionless := DX * DX + Upper_Y * Upper_Y;
               Lower_Gap : constant Dimensionless := DX * DX + Lower_Y * Lower_Y;
            begin
               pragma Assert (abs DX <= 2.0**22 and then abs Upper_Y <= 2.0**22 and then abs Lower_Y <= 2.0**22);
               if not (Upper_Gap >= Root_Gap_Floor) or else not (Lower_Gap >= Root_Gap_Floor) then
                  S_Re := 0.0;
                  S_Im := 0.0;
                  Separated := False;
                  return;
               end if;
               declare
                  Upper     : constant Dimensionless := 1.0 / Upper_Gap;
                  Lower     : constant Dimensionless := 1.0 / Lower_Gap;
                  Real_Term : constant Dimensionless := DX * (Upper + Lower);
                  Imaginary : constant Dimensionless := Upper_Y * Upper + Lower_Y * Lower;
               begin
                  pragma Assert (Upper >= 0.0 and then Upper <= 2.0**300);
                  pragma Assert (Lower >= 0.0 and then Lower <= 2.0**300);
                  pragma Assert (abs Real_Term <= Root_Term_Limit);
                  pragma Assert (abs Imaginary <= Root_Term_Limit);
                  Lemma_Root_Sum_Grows (S_Re, Real_Term, J);
                  Lemma_Root_Sum_Grows (S_Im, Imaginary, J);
                  S_Re := S_Re + Real_Term;
                  S_Im := S_Im - Imaginary;
               end;
            end;
         else
            Lemma_Root_Sum_Stays (S_Re, J);
            Lemma_Root_Sum_Stays (S_Im, J);
         end if;
      end loop;
      pragma
        Assert
          (abs S_Re <= Dimensionless (Half + 1) * Root_Term_Limit
           and then abs S_Im <= Dimensionless (Half + 1) * Root_Term_Limit);
      Lemma_Root_Sum_Stays (S_Re, Half + 1);
      Lemma_Root_Sum_Stays (S_Im, Half + 1);
   end Sum_Over_Other_Roots;

   procedure Aberth_Correction
     (P_Re, P_Im, D_Re, D_Im, S_Re, S_Im : Dimensionless; W_Re, W_Im : out Dimensionless; Usable : out Boolean)
   with
     Pre  => abs S_Re <= Root_Sum_Limit and then abs S_Im <= Root_Sum_Limit,
     Post => abs W_Re <= Root_Correction_Limit and then abs W_Im <= Root_Correction_Limit;
   pragma Inline (Aberth_Correction);
   --  Aberth's correction W = N/(1 - N·S) of an iterate from the polynomial's value P there, its derivative D and
   --  the sum S over the other roots, where N = P/D is Newton's correction. Usable is False, and the correction
   --  zero, where a value is beyond the limits which keep this arithmetic finite or a divisor is below its floor.

   procedure Aberth_Correction
     (P_Re, P_Im, D_Re, D_Im, S_Re, S_Im : Dimensionless; W_Re, W_Im : out Dimensionless; Usable : out Boolean)
   is
      pragma Suppress (Overflow_Check);
   begin
      W_Re := 0.0;
      W_Im := 0.0;
      Usable := False;
      if not (abs P_Re <= Root_Value_Limit
              and then abs P_Im <= Root_Value_Limit
              and then abs D_Re <= Root_Value_Limit
              and then abs D_Im <= Root_Value_Limit)
      then
         return;
      end if;
      declare
         D_Square : constant Dimensionless := D_Re * D_Re + D_Im * D_Im;
         N_Re_Top : constant Dimensionless := P_Re * D_Re + P_Im * D_Im;
         N_Im_Top : constant Dimensionless := P_Im * D_Re - P_Re * D_Im;
      begin
         pragma Assert (D_Square >= 0.0 and then D_Square <= 2.0**601);
         pragma Assert (abs N_Re_Top <= 2.0**601 and then abs N_Im_Top <= 2.0**601);
         if not (D_Square >= Root_Slope_Floor) then
            return;
         end if;
         declare
            --  Newton's correction N = P/D.
            N_Re : constant Dimensionless := N_Re_Top / D_Square;
            N_Im : constant Dimensionless := N_Im_Top / D_Square;
         begin
            if not (abs N_Re <= Newton_Limit and then abs N_Im <= Newton_Limit) then
               return;
            end if;
            declare
               Q_Re_Product : constant Dimensionless := N_Re * S_Re - N_Im * S_Im;
               Q_Im_Product : constant Dimensionless := N_Re * S_Im + N_Im * S_Re;
               Q_Re         : constant Dimensionless := 1.0 - Q_Re_Product;
               Q_Im         : constant Dimensionless := -Q_Im_Product;
               Q_Square     : constant Dimensionless := Q_Re * Q_Re + Q_Im * Q_Im;
               W_Re_Top     : constant Dimensionless := N_Re * Q_Re + N_Im * Q_Im;
               W_Im_Top     : constant Dimensionless := N_Im * Q_Re - N_Re * Q_Im;
            begin
               pragma Assert (abs Q_Re_Product <= 2.0**478 and then abs Q_Im_Product <= 2.0**478);
               pragma Assert (abs Q_Re <= 2.0**479 and then abs Q_Im <= 2.0**479);
               pragma Assert (Q_Square >= 0.0 and then Q_Square <= 2.0**959);
               pragma Assert (abs W_Re_Top <= 2.0**630 and then abs W_Im_Top <= 2.0**630);
               if not (Q_Square >= Root_Gap_Floor) then
                  return;
               end if;
               W_Re := W_Re_Top / Q_Square;
               W_Im := W_Im_Top / Q_Square;
               Usable := True;
            end;
         end;
      end;
   end Aberth_Correction;

   procedure Set_Root
     (Real_Parts, Imaginary_Parts : in out Conjugate_Root_Parts; K : Conjugate_Root_Index; X, Y : Dimensionless)
   with
     Pre  =>
       Are_Bounded (Real_Parts, Imaginary_Parts)
       and then abs X <= Root_Iterate_Limit
       and then abs Y <= Root_Iterate_Limit,
     Post => Are_Bounded (Real_Parts, Imaginary_Parts) and then Real_Parts (K) = X and then Imaginary_Parts (K) = Y;
   pragma Inline (Set_Root);
   --  Store an iterate which is within the limit that the others are within.

   procedure Set_Root
     (Real_Parts, Imaginary_Parts : in out Conjugate_Root_Parts; K : Conjugate_Root_Index; X, Y : Dimensionless) is
   begin
      Real_Parts (K) := X;
      Imaginary_Parts (K) := Y;
   end Set_Root;

   procedure Starting_Point
     (Centre, Radius : Dimensionless; K : Conjugate_Root_Index; Half : Positive; X, Y : out Dimensionless)
   with
     Pre  =>
       abs Centre <= Root_Limit
       and then Radius > 0.0
       and then Radius <= Root_Limit
       and then K <= Half
       and then Half <= Conjugate_Root_Index'Last,
     Post => abs X <= Root_Iterate_Limit and then abs Y <= Root_Iterate_Limit;
   --  The starting iterate of root K of Half on the circle of Radius about Centre. The phase offset keeps the
   --  start away from any symmetry of the roots.

   procedure Starting_Point
     (Centre, Radius : Dimensionless; K : Conjugate_Root_Index; Half : Positive; X, Y : out Dimensionless)
   is
      Angle  : constant Dimensionless :=
        Dimensionless (Ada.Numerics.Pi) * (Dimensionless (K) - 0.63) / Dimensionless (Half);
      Cosine : constant Dimensionless := Dimensionless_Math.Cos (Angle);
      Sine   : constant Dimensionless := Dimensionless_Math.Sin (Angle);
   begin
      pragma Assert (abs Cosine <= 1.0 and then abs Sine <= 1.0);
      X := Centre + Radius * Cosine;
      Y := Radius * Sine;
   end Starting_Point;

   procedure Improve_Root
     (Coefficients                : Root_Polynomial;
      Degree                      : Natural;
      Half                        : Natural;
      K                           : Conjugate_Root_Index;
      Real_Parts, Imaginary_Parts : in out Conjugate_Root_Parts;
      Settled, Usable             : out Boolean)
   with
     Pre  =>
       Degree in 1 .. Maximum_Rational_Degree
       and then Is_Tame (Coefficients, Degree)
       and then Half <= Conjugate_Root_Index'Last
       and then Are_Bounded (Real_Parts, Imaginary_Parts),
     Post => Are_Bounded (Real_Parts, Imaginary_Parts);
   pragma Inline (Improve_Root);
   --  One Aberth step of root K. Settled is True if its residual is within the rounding error of its own
   --  evaluation, in which case it is not moved, or if the step moved it by a negligible distance. Usable is False
   --  where the iteration cannot go on: the iterate is on or below the real axis or coincides with another, the
   --  derivative vanishes, or the step leaves the root limit.

   procedure Improve_Root
     (Coefficients                : Root_Polynomial;
      Degree                      : Natural;
      Half                        : Natural;
      K                           : Conjugate_Root_Index;
      Real_Parts, Imaginary_Parts : in out Conjugate_Root_Parts;
      Settled, Usable             : out Boolean)
   is
      pragma Suppress (Overflow_Check);
      X          : constant Dimensionless := Real_Parts (K);
      Y          : constant Dimensionless := Imaginary_Parts (K);
      Square_Sum : Dimensionless;
      Modulus    : Dimensionless;
      P_Re, P_Im : Dimensionless;
      D_Re, D_Im : Dimensionless;
      Size       : Dimensionless;
   begin
      Settled := False;
      Usable := False;
      pragma Assert (abs X <= Root_Iterate_Limit and then abs Y <= Root_Iterate_Limit);
      --  An iterate this close to the real axis, or below it, is not of a conjugate pair.
      if not (Y >= Root_Gap_Floor) then
         return;
      end if;
      Square_Sum := X * X + Y * Y;
      pragma Assert (Square_Sum >= 0.0 and then Square_Sum <= 2.0**43);
      Stereographic_Kernel.Elementary.Lemma_Sqrt_Is_At_Most (Square_Sum, 2.0**22);
      Modulus := Dimensionless_Math.Sqrt (Square_Sum);
      Evaluate_Root_Polynomial (Coefficients, Degree, X, Y, Modulus, P_Re, P_Im, D_Re, D_Im, Size);
      if abs P_Re + abs P_Im <= 8.0 * Dimensionless'Model_Epsilon * Size then
         --  The residual is within the rounding error of its own evaluation.
         Settled := True;
         Usable := True;
         return;
      end if;
      declare
         S_Re, S_Im : Dimensionless;
         W_Re, W_Im : Dimensionless;
         Separated  : Boolean;
         Corrected  : Boolean;
      begin
         Sum_Over_Other_Roots (Real_Parts, Imaginary_Parts, Half, K, X, Y, S_Re, S_Im, Separated);
         if not Separated then
            return;
         end if;
         Aberth_Correction (P_Re, P_Im, D_Re, D_Im, S_Re, S_Im, W_Re, W_Im, Corrected);
         if not Corrected then
            --  Coincident iterates or a vanishing derivative, which left an infinity or a NaN before these were
            --  tested for.
            return;
         end if;
         declare
            New_X : constant Dimensionless := X - W_Re;
            --  The root set is symmetric about the real axis, so a root which crosses it is replaced by its
            --  conjugate.
            New_Y : constant Dimensionless := abs (Y - W_Im);
         begin
            if not (abs New_X <= Root_Limit and then New_Y > 0.0 and then New_Y <= Root_Limit) then
               return;
            end if;
            Set_Root (Real_Parts, Imaginary_Parts, K, New_X, New_Y);
            Settled := abs W_Re + abs W_Im <= 1.0E-14 * Dimensionless'Max (1.0, Modulus);
            Usable := True;
         end;
      end;
   end Improve_Root;

   function Iterate_Conjugate_Roots
     (Coefficients : Root_Polynomial; Degree : Natural; Real_Parts, Imaginary_Parts : out Conjugate_Root_Parts)
      return Boolean
   is
      pragma Suppress (Overflow_Check);
      --  The limits keep every intermediate finite, as the contracts of the steps above state.
      Maximum_Iterations : constant := 40;
      Half               : constant Natural := Degree / 2;
      Settled            : array (Conjugate_Root_Index) of Boolean := [others => False];
      Leading            : constant Dimensionless := Coefficients (Degree);
      Centre, Radius     : Dimensionless;
      Value              : Dimensionless;
   begin
      Real_Parts := [others => 0.0];
      Imaginary_Parts := [others => 0.0];
      if Degree = 0 or else Degree mod 2 /= 0 or else Leading = 0.0 then
         return False;
      end if;
      for Index in 0 .. Degree loop
         pragma
           Loop_Invariant (for all Earlier in 0 .. Index - 1 => abs Coefficients (Earlier) <= Root_Coefficient_Limit);
         if not (abs Coefficients (Index) <= Root_Coefficient_Limit) then
            return False;
         end if;
      end loop;
      pragma Assert (Is_Tame (Coefficients, Degree));
      if not (abs Leading >= Root_Leading_Floor) then
         return False;
      end if;

      --  Start on a circle about the roots' centroid whose radius is the geometric mean of their distances
      --  from it, |D(c)/aₙ|^(1/n). The phase offset keeps the start away from any symmetry of the roots.
      declare
         Divisor : constant Dimensionless := Dimensionless (Degree) * Leading;
      begin
         pragma Assert (abs Divisor >= Root_Leading_Floor);
         pragma Assert (abs Coefficients (Degree - 1) <= Root_Coefficient_Limit);
         Centre := -Coefficients (Degree - 1) / Divisor;
      end;
      if not (abs Centre <= Root_Limit) then
         return False;
      end if;
      Value := Leading;
      for Index in reverse 0 .. Degree - 1 loop
         pragma Loop_Invariant (abs Value <= Horner_Limit (Degree - 1 - Index));
         Lemma_Horner_Limit_Grows (Degree - 1 - Index);
         pragma Assert (abs Coefficients (Index) <= Root_Coefficient_Limit);
         Lemma_Horner_Plain (Value, Centre, Coefficients (Index), Horner_Limit (Degree - 1 - Index));
         Value := Value * Centre + Coefficients (Index);
      end loop;
      pragma Assert (abs Value <= Horner_Result_Limit);
      Value := abs (Value / Leading);
      if not (Value > 0.0 and then Value <= Dimensionless'Last) then
         return False;
      end if;
      Stereographic_Kernel.Elementary.Lemma_Log_Is_Bounded (Value);
      declare
         Exponent : constant Dimensionless := Dimensionless_Math.Log (Value) / Dimensionless (Degree);
      begin
         --  A radius is at most the root limit, whose logarithm is below fourteen, and a polynomial whose roots
         --  are closer than e⁻⁶⁴ to their centroid is left to the eigenvalue solve.
         if not (abs Exponent <= Stereographic_Kernel.Elementary.Exp_Argument_Max) then
            return False;
         end if;
         Radius := Stereographic_Kernel.Elementary.Bounded_Exp (Exponent);
      end;
      if not (Radius > 0.0 and then Radius <= Root_Limit) then
         return False;
      end if;
      for K in 1 .. Half loop
         pragma Loop_Invariant (Are_Bounded (Real_Parts, Imaginary_Parts));
         declare
            Start_X, Start_Y : Dimensionless;
         begin
            Starting_Point (Centre, Radius, K, Half, Start_X, Start_Y);
            Set_Root (Real_Parts, Imaginary_Parts, K, Start_X, Start_Y);
         end;
      end loop;

      for Iteration in 1 .. Maximum_Iterations loop
         pragma Loop_Invariant (Are_Bounded (Real_Parts, Imaginary_Parts));
         for K in 1 .. Half loop
            pragma Loop_Invariant (Are_Bounded (Real_Parts, Imaginary_Parts));
            if not Settled (K) then
               declare
                  Has_Settled, Usable : Boolean;
               begin
                  Improve_Root (Coefficients, Degree, Half, K, Real_Parts, Imaginary_Parts, Has_Settled, Usable);
                  if not Usable then
                     return False;
                  end if;
                  Settled (K) := Has_Settled;
               end;
            end if;
         end loop;
         exit when (for all K in 1 .. Half => Settled (K));
      end loop;

      for K in 1 .. Half loop
         --  A pair this close to the real axis would later be classified as two real poles; leave such a
         --  polynomial to the eigenvalue solve.
         pragma
           Assert (abs Real_Parts (K) <= Root_Iterate_Limit and then abs Imaginary_Parts (K) <= Root_Iterate_Limit);
         if not Settled (K)
           or else Imaginary_Parts (K) <= 1.0E-9 * Dimensionless'Max (1.0, abs Real_Parts (K) + Imaginary_Parts (K))
         then
            return False;
         end if;
      end loop;
      return True;
   end Iterate_Conjugate_Roots;

   function Build_Rational_Representation
     (Curve                  : in out Stereographic_Curve;
      Power_Coefficients     : Power_Chart;
      Maximum_Position_Error : Length;
      Start_Jet, Finish_Jet  : Endpoint_Tangent_Jet) return Realtime_Compilation_Status
   is
      subtype Polynomial_Index is Natural range 0 .. Maximum_Rational_Degree;
      subtype Real_Polynomial is Root_Polynomial;

      package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Dimensionless);
      use Complex_Types;

      type Complex_Root_Array is array (Rational_Degree_Slot) of Complex;
      type Temporary_Real_Poles is array (Rational_Degree_Slot) of Dimensionless;
      type Temporary_Complex_Poles is array (Rational_Pair_Index) of Complex;

      R2, Denominator        : Real_Polynomial := [others => 0.0];
      Local_Numerators       : array (Frame_Component_Index) of Real_Polynomial := [others => [others => 0.0]];
      Numerators             : array (Axis_Name) of Real_Polynomial := [others => [others => 0.0]];
      Roots                  : Complex_Root_Array := [others => Compose_From_Cartesian (0.0, 0.0)];
      Denominator_Derivative : Real_Polynomial := [others => 0.0];
      Pole_Derivatives       : Complex_Root_Array := [others => Compose_From_Cartesian (0.0, 0.0)];
      Real_Poles             : Temporary_Real_Poles := [others => 0.0];
      Pair_Poles             : Temporary_Complex_Poles := [others => Compose_From_Cartesian (0.0, 0.0)];
      Real_Pole_Count        : Natural range 0 .. Maximum_Rational_Degree := 0;
      Pair_Count             : Natural range 0 .. Maximum_Rational_Degree / 2 := 0;
      Chart_Degree           : Natural range 0 .. Fixed_Chart_Degree := 0;
      Denominator_Degree     : Natural range 0 .. Maximum_Rational_Degree := 0;
      Cache                  : Rational_Antiderivative := (others => <>);

      function Evaluate_Polynomial (Coefficients : Real_Polynomial; Degree : Natural; Value : Complex) return Complex;
      --  Evaluate a real-coefficient polynomial at a complex value using Horner's method.

      function Evaluate_Polynomial (Coefficients : Real_Polynomial; Degree : Natural; Value : Complex) return Complex
      is
         Result : Complex := Compose_From_Cartesian (Coefficients (Degree), 0.0);
      begin
         if Degree = 0 then
            return Result;
         end if;

         --  The complex product and sum written out, as the same real operations in the same order. An overflow
         --  leaves an infinity or a NaN in every later step, so a finite result is the one which the complex
         --  operators give, without a call and an overflow test for each of them.
         declare
            pragma Suppress (Overflow_Check);
            X    : constant Dimensionless := Value.Re;
            Y    : constant Dimensionless := Value.Im;
            P_Re : Dimensionless := Coefficients (Degree);
            P_Im : Dimensionless := 0.0;
            Next : Dimensionless;
         begin
            for Index in reverse 0 .. Degree - 1 loop
               Next := (P_Re * X - P_Im * Y) + Coefficients (Index);
               P_Im := (P_Re * Y + P_Im * X) + 0.0;
               P_Re := Next;
            end loop;
            if abs P_Re <= Dimensionless'Last and then abs P_Im <= Dimensionless'Last then
               return Compose_From_Cartesian (P_Re, P_Im);
            end if;
         end;

         --  The complex product rescales a component which overflows, so leave such a value to it.
         for Index in reverse 0 .. Degree - 1 loop
            Result := Result * Value + Compose_From_Cartesian (Coefficients (Index), 0.0);
         end loop;
         return Result;
      end Evaluate_Polynomial;

      function Complex_Is_Finite (Value : Complex) return Boolean
      is (Value.Re >= -Dimensionless'Last
          and then Value.Re <= Dimensionless'Last
          and then Value.Im >= -Dimensionless'Last
          and then Value.Im <= Dimensionless'Last);

      function Root_Scale (Root : Complex) return Dimensionless
      is (Dimensionless'Max (1.0, Dimensionless (abs Root)));

      function Polynomial_Scale_At
        (Coefficients : Real_Polynomial; Degree : Natural; Root : Complex) return Dimensionless;
      --  Estimate the polynomial evaluation scale at a candidate root.

      function Polynomial_Scale_At
        (Coefficients : Real_Polynomial; Degree : Natural; Root : Complex) return Dimensionless
      is
         Scale  : Dimensionless := 0.0;
         Power  : Dimensionless := 1.0;
         Radius : constant Dimensionless := Dimensionless (abs Root);
      begin
         for Index in 0 .. Degree loop
            Scale := Scale + abs Coefficients (Index) * Power;
            Power := Power * Radius;
         end loop;
         return Dimensionless'Max (1.0, Scale);
      end Polynomial_Scale_At;

      function Distance_From_Unit_Interval (Root : Complex) return Dimensionless;
      --  Return the distance from a complex root to the real unit interval.

      function Distance_From_Unit_Interval (Root : Complex) return Dimensionless is
         Root_Real     : constant Dimensionless := Dimensionless (Re (Root));
         Real_Distance : constant Dimensionless :=
           (if Root_Real < 0.0 then -Root_Real elsif Root_Real > 1.0 then Root_Real - 1.0 else 0.0);
      begin
         return
           Dimensionless_Math.Sqrt
             (Real_Distance * Real_Distance + Dimensionless (Im (Root)) * Dimensionless (Im (Root)));
      end Distance_From_Unit_Interval;

      function Find_Roots
        (Coefficients : Real_Polynomial; Degree : Natural; Result : out Complex_Root_Array) return Boolean
      with Side_Effects;
      --  Find and validate all complex roots of the supplied real polynomial.

      function Find_Roots
        (Coefficients : Real_Polynomial; Degree : Natural; Result : out Complex_Root_Array) return Boolean
      is
         Matrix                      :
           Prunt.LAPACK.Real_Matrix (1 .. Maximum_Rational_Degree, 1 .. Maximum_Rational_Degree) :=
             [others => [others => 0.0]];
         Real_Roots, Imaginary_Roots : Prunt.LAPACK.Real_Vector (1 .. Maximum_Rational_Degree) := [others => 0.0];
         Info                        : Integer;
         Leading                     : constant Dimensionless := Coefficients (Degree);
      begin
         Result := [others => Compose_From_Cartesian (0.0, 0.0)];
         if Degree = 0 then
            return True;
         elsif Leading = 0.0 or else not Is_Finite (Leading) then
            return False;
         end if;

         --  The eigenvalues of the monic companion matrix are the roots of
         --
         --     aₙxⁿ + aₙ₋₁xⁿ⁻¹ + … + a₀.
         for Row in 2 .. Degree loop
            Matrix (Row, Row - 1) := 1.0;
         end loop;
         for Row in 1 .. Degree loop
            declare
               Coefficient : constant Dimensionless := Coefficients (Row - 1) / Leading;
            begin
               if not Is_Finite (Coefficient) then
                  return False;
               end if;
               Matrix (Row, Degree) := Prunt.LAPACK.Real (-Coefficient);
            end;
         end loop;

         --  The SPARK port of DGEEV ('N', 'N'). Info is nonzero when it did not converge or gave up on a magnitude
         --  it can not bound (docs/spark_lapack.md); on success conjugate pairs are stored together, the root in
         --  the upper half plane first.
         Prunt.LAPACK.Eigenvalues.Solve (Degree, Matrix, Real_Roots, Imaginary_Roots, Info);
         if Info /= 0 then
            return False;
         end if;

         for Index in 1 .. Degree loop
            Result (Index) :=
              Compose_From_Cartesian (Dimensionless (Real_Roots (Index)), Dimensionless (Imaginary_Roots (Index)));
         end loop;
         return True;
      exception
         when Constraint_Error =>
            Result := [others => Compose_From_Cartesian (0.0, 0.0)];
            return False;
      end Find_Roots;

      function Find_Roots_By_Iteration
        (Coefficients : Real_Polynomial; Degree : Natural; Result : out Complex_Root_Array) return Boolean
      with Side_Effects;
      --  Iterate_Conjugate_Roots with each pair returned as two adjacent exact conjugates, the upper one first.

      function Find_Roots_By_Iteration
        (Coefficients : Real_Polynomial; Degree : Natural; Result : out Complex_Root_Array) return Boolean
      is
         Real_Parts, Imaginary_Parts : Conjugate_Root_Parts;
         Settled                     : Boolean;
      begin
         Result := [others => Compose_From_Cartesian (0.0, 0.0)];
         Settled := Iterate_Conjugate_Roots (Coefficients, Degree, Real_Parts, Imaginary_Parts);
         if not Settled then
            return False;
         end if;
         for K in 1 .. Degree / 2 loop
            Result (2 * K - 1) := Compose_From_Cartesian (Real_Parts (K), Imaginary_Parts (K));
            Result (2 * K) := Compose_From_Cartesian (Real_Parts (K), -Imaginary_Parts (K));
         end loop;
         return True;
      end Find_Roots_By_Iteration;

      function Ideal_Axis_Is_Structurally_Zero (Axis : Axis_Name) return Boolean;
      --  Test whether the ideal tangent numerator is identically zero for an axis.

      function Ideal_Axis_Is_Structurally_Zero (Axis : Axis_Name) return Boolean is
      begin
         if Curve.Frame (0) (Axis) /= 0.0 then
            return False;
         end if;
         for Component in Chart_Component_Index loop
            if Curve.Frame (Component) (Axis) /= 0.0
              and then
                (for some Coefficient in Chart_Coefficient_Index => Curve.Coefficients (Coefficient, Component) /= 0.0)
            then
               return False;
            end if;
         end loop;
         return True;
      exception
         when Constraint_Error =>
            return False;
      end Ideal_Axis_Is_Structurally_Zero;

   begin
      --  Reject malformed inputs before any polynomial or root arithmetic can contaminate the output cache.
      if Curve.Evaluator_Data.Kind /= Positive_Curve_Kind
        or else Curve.Evaluator_Data.Length_Value <= 0.0 * mm
        or else Maximum_Position_Error <= 0.0 * mm
        or else Maximum_Position_Error > Length'Last
      then
         return Realtime_Compilation_Numerically_Unsafe;
      end if;

      for Axis in Axis_Name loop
         Curve.Structurally_Constant_Axes (Axis) :=
           Curve.Evaluator_Data.Start_Point (Axis) = Curve.Evaluator_Data.Finish_Point (Axis)
           and then Ideal_Axis_Is_Structurally_Zero (Axis);
      end loop;

      --  The loop invariants of this function stop GNATprove from unrolling the outer loops of its nests. Unrolled,
      --  the nests copy their bodies hundreds of times and the proof of the function exhausts 31 GB of memory.
      for Degree in reverse 0 .. Fixed_Chart_Degree loop
         pragma Loop_Invariant (True);
         declare
            Row_Norm : Dimensionless := 0.0;
         begin
            for Component in Chart_Component_Index loop
               Row_Norm := Row_Norm + Power_Coefficients (Degree, Component)**2;
            end loop;
            if Row_Norm > 0.0 then
               Chart_Degree := Degree;
               exit;
            end if;
         end;
      end loop;
      Denominator_Degree := 2 * Chart_Degree;

      --  The inverse-stereographic tangent in chart coordinate V is
      --
      --     T(V) = F·((1 - Y·Y), 2Y)/(1 + Y·Y).
      --
      --  Build its common denominator D = 1 + Y·Y and one numerator Nₐ for each physical axis.
      for Component in Chart_Component_Index loop
         pragma Loop_Invariant (True);
         for Left in 0 .. Chart_Degree loop
            pragma Loop_Invariant (True);
            for Right in 0 .. Chart_Degree loop
               R2 (Left + Right) :=
                 R2 (Left + Right) + Power_Coefficients (Left, Component) * Power_Coefficients (Right, Component);
            end loop;
         end loop;
      end loop;
      Denominator := R2;
      Denominator (0) := Denominator (0) + 1.0;
      for Degree in Polynomial_Index loop
         Local_Numerators (0) (Degree) := -R2 (Degree);
      end loop;
      Local_Numerators (0) (0) := Local_Numerators (0) (0) + 1.0;
      for Component in Chart_Component_Index loop
         pragma Loop_Invariant (True);
         for Degree in 0 .. Chart_Degree loop
            Local_Numerators (Component) (Degree) := 2.0 * Power_Coefficients (Degree, Component);
         end loop;
      end loop;
      for Axis in Axis_Name loop
         pragma Loop_Invariant (True);
         for Component in Frame_Component_Index loop
            pragma Loop_Invariant (True);
            for Degree in 0 .. Denominator_Degree loop
               Numerators (Axis) (Degree) :=
                 Numerators (Axis) (Degree) + Curve.Frame (Component) (Axis) * Local_Numerators (Component) (Degree);
            end loop;
         end loop;
      end loop;

      if Denominator_Degree > 0 then
         for Degree in 1 .. Denominator_Degree loop
            Denominator_Derivative (Degree - 1) := Dimensionless (Degree) * Denominator (Degree);
         end loop;
      end if;

      declare
         type Root_Validation is (Roots_Are_Valid, Roots_Are_Inaccurate, Roots_Are_Unusable);
         --  Inaccurate roots fail a test which depends on how they were found: the residual, or the separation of
         --  two roots. Unusable roots are not finite or lie too close to the evaluation interval, which no other
         --  method could change.

         procedure Refine_Roots;
         --  Polish the eigenvalue solve's roots with Newton's method.

         function Validate_Roots return Root_Validation;
         --  Check every root before the partial fractions divide by D′ at it.

         procedure Refine_Roots is
            Conjugate_Next : Boolean := False;
         begin
            for Index in 1 .. Denominator_Degree loop
               if Conjugate_Next then
                  Roots (Index) := Conjugate (Roots (Index - 1));
                  Conjugate_Next := False;
               else
                  --  A real polynomial has conjugate roots. Find_Roots and Find_Roots_By_Iteration return conjugate
                  --  pairs together; only share a refinement when the supplied pair is exactly conjugate. Both roots
                  --  are still validated below.
                  Conjugate_Next :=
                    Index < Denominator_Degree
                    and then Im (Roots (Index)) > 0.0
                    and then Roots (Index + 1) = Conjugate (Roots (Index));
                  --  Up to three Newton steps reduce the companion eigenvalue's polynomial residual, accepting
                  --  only a step which improves |D(p)|.
                  for Pass in 1 .. 3 loop
                     exit when not Complex_Is_Finite (Roots (Index)) or else abs Roots (Index) > 1.0E6;
                     declare
                        Value      : constant Complex :=
                          Evaluate_Polynomial (Denominator, Denominator_Degree, Roots (Index));
                        Derivative : constant Complex :=
                          Evaluate_Polynomial (Denominator_Derivative, Denominator_Degree - 1, Roots (Index));
                     begin
                        exit when
                          not Complex_Is_Finite (Value)
                          or else not Complex_Is_Finite (Derivative)
                          or else abs Derivative <= 64.0 * Dimensionless'Model_Epsilon;
                        declare
                           Trial : constant Complex := Roots (Index) - Value / Derivative;
                        begin
                           if Complex_Is_Finite (Trial)
                             and then abs Evaluate_Polynomial (Denominator, Denominator_Degree, Trial) < abs Value
                           then
                              Roots (Index) := Trial;
                           else
                              --  Retrying an unchanged root would repeat the same rejected step.
                              exit;
                           end if;
                        end;
                     end;
                  end loop;
               end if;
            end loop;
         end Refine_Roots;

         function Validate_Roots return Root_Validation is
            type Pair_Role is (Unpaired, Upper_Member, Conjugate_Member);
            --  The two members of a pair of roots which are exact conjugates of one another, stored together with
            --  the member in the upper half-plane first, as the simultaneous iteration returns every pair.

            Result : Root_Validation := Roots_Are_Valid;
            Roles  : array (Rational_Degree_Slot) of Pair_Role := [others => Unpaired];
            Scales : array (Rational_Degree_Slot) of Dimensionless := [others => 1.0];
         begin
            for Index in 2 .. Denominator_Degree loop
               if Roles (Index - 1) = Unpaired
                 and then Im (Roots (Index - 1)) > 0.0
                 and then Roots (Index) = Conjugate (Roots (Index - 1))
               then
                  Roles (Index - 1) := Upper_Member;
                  Roles (Index) := Conjugate_Member;
               end if;
            end loop;

            for Index in 1 .. Denominator_Degree loop
               --  Arithmetic on a conjugate gives the conjugate of the result exactly, so every quantity tested
               --  here is the same for both members of a pair and is formed for the first one only.
               if Roles (Index) = Conjugate_Member then
                  Scales (Index) := Scales (Index - 1);
               else
                  declare
                     Root_Is_Finite    : constant Boolean := Complex_Is_Finite (Roots (Index));
                     Interval_Distance : constant Dimensionless := Distance_From_Unit_Interval (Roots (Index));
                     Residual          : constant Dimensionless :=
                       Dimensionless (abs Evaluate_Polynomial (Denominator, Denominator_Degree, Roots (Index)));
                     Residual_Scale    : constant Dimensionless :=
                       Polynomial_Scale_At (Denominator, Denominator_Degree, Roots (Index));
                  begin
                     --  Every pole must be finite, separated from the evaluation interval, and an accurate root
                     --  of D.
                     if not Root_Is_Finite or else Interval_Distance < 2.0E-9 then
                        return Roots_Are_Unusable;
                     elsif Residual > 2.0E-8 * Residual_Scale then
                        Result := Roots_Are_Inaccurate;
                     end if;
                     Scales (Index) := Root_Scale (Roots (Index));
                  end;
               end if;
            end loop;

            for Index in 1 .. Denominator_Degree loop
               for Other in Index + 1 .. Denominator_Degree loop
                  --  The residue formula divides by D′(p), so repeated or numerically merged roots are unsupported.
                  --  Between two exact pairs only their upper members need comparing: the two conjugates are the
                  --  same distance apart, and an upper member is at least as far from the other pair's conjugate.
                  if (Roles (Index) = Unpaired or else Roles (Other) = Unpaired)
                    or else
                      (Roles (Index) = Upper_Member and then (Roles (Other) = Upper_Member or else Other = Index + 1))
                  then
                     if Dimensionless (abs (Roots (Index) - Roots (Other)))
                       < 2.0E-13 * Dimensionless'Max (Scales (Index), Scales (Other))
                     then
                        Result := Roots_Are_Inaccurate;
                     end if;
                  end if;
               end loop;
            end loop;
            return Result;
         end Validate_Roots;

         Validation  : Root_Validation;
         Roots_Found : Boolean;
      begin
         --  The simultaneous iteration is several times cheaper than the eigenvalue solve and settles each root to
         --  the rounding error of the polynomial itself, so its roots need no refinement. It has no error estimate
         --  of its own, so roots which fail validation for a reason the eigenvalue solve could avoid, such as two
         --  iterates which settled on one root, are found again by that solve.
         Roots_Found := Find_Roots_By_Iteration (Denominator, Denominator_Degree, Roots);
         if Roots_Found then
            Validation := Validate_Roots;
         else
            Validation := Roots_Are_Inaccurate;
         end if;
         if Validation = Roots_Are_Inaccurate then
            Roots_Found := Find_Roots (Denominator, Denominator_Degree, Roots);
            if not Roots_Found then
               return Realtime_Representation_Insufficient;
            end if;
            Refine_Roots;
            Validation := Validate_Roots;
         end if;
         if Validation /= Roots_Are_Valid then
            return Realtime_Representation_Insufficient;
         end if;
      end;

      declare
         Used : array (Rational_Degree_Slot) of Boolean := [others => False];
      begin
         --  Classify nearly real roots directly and pair every remaining root with its numerical conjugate. Average
         --  each pair to restore an exactly real quadratic representation.
         for Index in 1 .. Denominator_Degree loop
            if not Used (Index) then
               declare
                  Scale : constant Dimensionless := Root_Scale (Roots (Index));
               begin
                  if Dimensionless (abs Im (Roots (Index))) <= 1.0E-11 * Scale then
                     Real_Pole_Count := Real_Pole_Count + 1;
                     Real_Poles (Real_Pole_Count) := Dimensionless (Re (Roots (Index)));
                     Used (Index) := True;
                  else
                     declare
                        Target        : constant Complex :=
                          Compose_From_Cartesian (Re (Roots (Index)), -Im (Roots (Index)));
                        Best          : Natural := 0;
                        Best_Distance : Dimensionless := Dimensionless'Last;
                     begin
                        for Other in 1 .. Denominator_Degree loop
                           if Other /= Index
                             and then not Used (Other)
                             and then Dimensionless (abs (Roots (Other) - Target)) < Best_Distance
                           then
                              Best := Other;
                              Best_Distance := Dimensionless (abs (Roots (Other) - Target));
                           end if;
                        end loop;
                        if Best = 0
                          or else Best_Distance > 1.0E-7 * Scale
                          or else Pair_Count = Maximum_Rational_Degree / 2
                        then
                           return Realtime_Representation_Insufficient;
                        end if;
                        declare
                           Positive_Root  : constant Complex :=
                             (if Im (Roots (Index)) > 0.0 then Roots (Index) else Roots (Best));
                           Negative_Root  : constant Complex :=
                             (if Im (Roots (Index)) > 0.0 then Roots (Best) else Roots (Index));
                           Real_Part      : constant Dimensionless :=
                             Dimensionless (0.5 * (Re (Positive_Root) + Re (Negative_Root)));
                           Imaginary_Part : constant Dimensionless :=
                             Dimensionless (0.5 * (Im (Positive_Root) - Im (Negative_Root)));
                        begin
                           if Imaginary_Part <= 1.0E-14 * Scale then
                              return Realtime_Representation_Insufficient;
                           end if;
                           Pair_Count := Pair_Count + 1;
                           Pair_Poles (Pair_Count) := Compose_From_Cartesian (Real_Part, Imaginary_Part);
                           Used (Index) := True;
                           Used (Best) := True;
                        end;
                     end;
                  end if;
               end;
            end if;
         end loop;
      end;

      if Real_Pole_Count + 2 * Pair_Count /= Denominator_Degree then
         return Realtime_Representation_Insufficient;
      end if;
      Cache.Real_Pole_Count := Real_Pole_Count;
      Cache.Pair_Count := Pair_Count;
      for Index in 1 .. Real_Pole_Count loop
         Cache.Pole_Slots (Index).Pole_Component := Real_Poles (Index);
      end loop;
      for Pair in 1 .. Pair_Count loop
         declare
            Real_Slot      : constant Rational_Degree_Slot :=
              Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair));
            Imaginary_Slot : constant Rational_Degree_Slot :=
              Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair));
         begin
            Cache.Pole_Slots (Real_Slot).Pole_Component := Dimensionless (Re (Pair_Poles (Pair)));
            Cache.Pole_Slots (Imaginary_Slot).Pole_Component := Dimensionless (Im (Pair_Poles (Pair)));
         end;
      end loop;

      --  D'(p) is shared by all axis residues at a pole. Evaluate and validate it once, including poles whose
      --  conjugate components were merged above; the residue numerator remains axis-specific.
      for Slot in 1 .. Real_Pole_Count + Pair_Count loop
         declare
            Root       : constant Complex :=
              (if Slot <= Real_Pole_Count
               then Compose_From_Cartesian (Real_Poles (Slot), 0.0)
               else Pair_Poles (Slot - Real_Pole_Count));
            Derivative : constant Complex :=
              Evaluate_Polynomial (Denominator_Derivative, Denominator_Degree - 1, Root);
         begin
            if not Complex_Is_Finite (Derivative)
              or else Dimensionless (abs Derivative) <= 64.0 * Dimensionless'Model_Epsilon
            then
               return Realtime_Representation_Insufficient;
            end if;
            Pole_Derivatives (Slot) := Derivative;
         end;
      end loop;

      --  Structurally constant axes retain their initially zero quotient and residues.
      for Axis in Axis_Name loop
         if not Curve.Structurally_Constant_Axes (Axis) then
            declare
               Remainder : Real_Polynomial := Numerators (Axis);
               Quotient  : Dimensionless;
            begin
               --  Numerator and denominator have equal degree, so polynomial division leaves one constant quotient.
               --  For each simple pole p, the partial-fraction residue is
               --
               --     r = Remainder(p)/D′(p).
               if Denominator_Degree = 0 then
                  Quotient := Remainder (0) / Denominator (0);
                  Remainder (0) := 0.0;
               else
                  Quotient := Remainder (Denominator_Degree) / Denominator (Denominator_Degree);
                  for Degree in 0 .. Denominator_Degree loop
                     Remainder (Degree) := Remainder (Degree) - Quotient * Denominator (Degree);
                  end loop;
               end if;
               Cache.Constant_Tangent (Axis) := Quotient;

               for Index in 1 .. Real_Pole_Count loop
                  declare
                     Root       : constant Complex := Compose_From_Cartesian (Real_Poles (Index), 0.0);
                     Derivative : constant Complex := Pole_Derivatives (Index);
                     Residue    : Complex;
                  begin
                     Residue := Evaluate_Polynomial (Remainder, Denominator_Degree - 1, Root) / Derivative;
                     if not Complex_Is_Finite (Residue) then
                        return Realtime_Representation_Insufficient;
                     end if;
                     Cache.Pole_Slots (Index).Residue_Component (Axis) := Dimensionless (Re (Residue));
                  end;
               end loop;
               for Pair in 1 .. Pair_Count loop
                  declare
                     Real_Slot      : constant Rational_Degree_Slot :=
                       Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair));
                     Imaginary_Slot : constant Rational_Degree_Slot :=
                       Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair));
                     Root           : constant Complex := Pair_Poles (Pair);
                     Derivative     : constant Complex := Pole_Derivatives (Real_Pole_Count + Pair);
                     Residue        : Complex;
                  begin
                     Residue := Evaluate_Polynomial (Remainder, Denominator_Degree - 1, Root) / Derivative;
                     if not Complex_Is_Finite (Residue) then
                        return Realtime_Representation_Insufficient;
                     end if;
                     Cache.Pole_Slots (Real_Slot).Residue_Component (Axis) := Dimensionless (Re (Residue));
                     Cache.Pole_Slots (Imaginary_Slot).Residue_Component (Axis) := Dimensionless (Im (Residue));
                  end;
               end loop;
            end;
         end if;
      end loop;
      if not Rational_Antiderivative_Is_Well_Formed (Cache) then
         return Realtime_Representation_Insufficient;
      end if;

      declare
         Warp            : constant Dimensionless := Curve.Warp_Factor;
         Warp_Difference : constant Dimensionless := Warp - 1.0;
      begin
         --  Convert the V-space partial fractions to physical U using V = W·U/(1 + (W - 1)·U). For a pole p and
         --  residue r,
         --
         --     pᵤ = p/(W - (W - 1)·p),
         --     rᵤ = W·r/(W - (W - 1)·p)²,
         --
         --  plus a constant tangent contribution (W - 1)·r/(W - (W - 1)·p).
         for Index in 1 .. Cache.Real_Pole_Count loop
            pragma Loop_Invariant (True);
            declare
               Pole     : constant Dimensionless := Cache.Pole_Slots (Index).Pole_Component;
               Scale    : constant Dimensionless := Warp - Warp_Difference * Pole;
               New_Pole : constant Dimensionless := Pole / Scale;
            begin
               if Scale = 0.0 or else not Is_Finite (Scale) or else not Is_Finite (New_Pole) then
                  return Realtime_Compilation_Numerically_Unsafe;
               end if;
               for Axis in Axis_Name loop
                  declare
                     Residue           : constant Dimensionless := Cache.Pole_Slots (Index).Residue_Component (Axis);
                     Constant_Addition : constant Dimensionless := Warp_Difference * Residue / Scale;
                     New_Residue       : constant Dimensionless := Warp * Residue / (Scale * Scale);
                  begin
                     Cache.Constant_Tangent (Axis) := Cache.Constant_Tangent (Axis) + Constant_Addition;
                     Cache.Pole_Slots (Index).Residue_Component (Axis) := New_Residue;
                  end;
               end loop;
               Cache.Pole_Slots (Index).Pole_Component := New_Pole;
            end;
         end loop;

         for Pair in 1 .. Cache.Pair_Count loop
            declare
               Real_Slot      : constant Rational_Degree_Slot :=
                 Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair));
               Imaginary_Slot : constant Rational_Degree_Slot :=
                 Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair));
               Pole           : constant Complex :=
                 Compose_From_Cartesian
                   (Cache.Pole_Slots (Real_Slot).Pole_Component, Cache.Pole_Slots (Imaginary_Slot).Pole_Component);
               Scale          : constant Complex :=
                 Compose_From_Cartesian (Warp, 0.0) - Compose_From_Cartesian (Warp_Difference, 0.0) * Pole;
               New_Pole       : constant Complex := Pole / Scale;
            begin
               if not Complex_Is_Finite (Scale)
                 or else abs Scale = 0.0
                 or else not Complex_Is_Finite (New_Pole)
                 or else Im (New_Pole) <= 0.0
               then
                  return Realtime_Compilation_Numerically_Unsafe;
               end if;
               for Axis in Axis_Name loop
                  declare
                     Residue           : constant Complex :=
                       Compose_From_Cartesian
                         (Cache.Pole_Slots (Real_Slot).Residue_Component (Axis),
                          Cache.Pole_Slots (Imaginary_Slot).Residue_Component (Axis));
                     Constant_Addition : constant Complex :=
                       Compose_From_Cartesian (Warp_Difference, 0.0) * Residue / Scale;
                     New_Residue       : constant Complex :=
                       Compose_From_Cartesian (Warp, 0.0) * Residue / (Scale * Scale);
                  begin
                     if not Complex_Is_Finite (New_Residue) or else not Complex_Is_Finite (Constant_Addition) then
                        return Realtime_Compilation_Numerically_Unsafe;
                     end if;
                     Cache.Constant_Tangent (Axis) :=
                       Cache.Constant_Tangent (Axis) + 2.0 * Dimensionless (Re (Constant_Addition));
                     Cache.Pole_Slots (Real_Slot).Residue_Component (Axis) := Dimensionless (Re (New_Residue));
                     Cache.Pole_Slots (Imaginary_Slot).Residue_Component (Axis) := Dimensionless (Im (New_Residue));
                  end;
               end loop;
               Cache.Pole_Slots (Real_Slot).Pole_Component := Dimensionless (Re (New_Pole));
               Cache.Pole_Slots (Imaginary_Slot).Pole_Component := Dimensionless (Im (New_Pole));
            end;
         end loop;

         if not Rational_Antiderivative_Is_Well_Formed (Cache) then
            return Realtime_Representation_Insufficient;
         end if;
         declare
            Retained_Certificate : Retained_Tangent_Bernstein_Certificate;
            Evidence             : Stereographic_Kernel.Certified_Evaluator.Certificate;
            Sampled              : Tangent_Certificate_Result;
            Certificate          : Tangent_Certificate_Result;
         begin
            Sampled := Sampled_Realtime_Tangent_Error (Curve, Cache, Denominator (0), Evidence);
            Certificate := Sampled;
            --  The sampled proof is far cheaper and normally certifies an error near roundoff. It is also the one
            --  whose bound is proved to hold, and whose evidence Create_Blend requires of every curve, so a cache
            --  which it does not certify is refused here. Run the Bernstein reconstruction as well only when the
            --  sampled bound leaves less than a sixteenth of the budget unproved; that reconstruction also retains
            --  the common-denominator form for the Bernstein derivative bounds.
            if Sampled.Status = Tangent_Was_Certified
              and then
                not (Sampled.Error_Bound
                     <= Dimensionless (Maximum_Position_Error / Curve.Evaluator_Data.Length_Value) / 16.0)
            then
               declare
                  Reconstructed : Tangent_Certificate_Result;
               begin
                  Reconstructed :=
                    Certified_Realtime_Tangent_Error
                      (Curve, Cache, Denominator (0), Maximum_Position_Error, Retained_Certificate);
                  --  The smaller error stands for the position budget. The reconstruction is not proved, so the
                  --  evidence keeps the sampled bound.
                  if Reconstructed.Status = Tangent_Was_Certified
                    and then Reconstructed.Error_Bound < Sampled.Error_Bound
                  then
                     Certificate := Reconstructed;
                  end if;
               end;
            end if;
            --  Do not install the realtime cache until interval arithmetic proves its tangent field close enough to
            --  the authoritative Bernstein chart over the complete curve.
            case Certificate.Status is
               when Tangent_Could_Not_Be_Certified            =>
                  return Realtime_Representation_Insufficient;

               when Tangent_Certificate_Is_Numerically_Unsafe =>
                  return Realtime_Compilation_Numerically_Unsafe;

               when Tangent_Was_Certified                     =>
                  declare
                     Physical_Error : constant Length :=
                       Up (Dimensionless (Curve.Evaluator_Data.Length_Value / mm) * Certificate.Error_Bound) * mm;
                  begin
                     if not Is_Finite (Dimensionless (Physical_Error / mm)) then
                        return Realtime_Compilation_Numerically_Unsafe;
                     elsif Physical_Error > Maximum_Position_Error then
                        return Realtime_Representation_Insufficient;
                     end if;
                     Curve.Evaluator_Data :=
                       (Kind                     => Positive_Curve_Kind,
                        Start_Point              => Curve.Evaluator_Data.Start_Point,
                        Finish_Point             => Curve.Evaluator_Data.Finish_Point,
                        Length_Value             => Curve.Evaluator_Data.Length_Value,
                        Antiderivative_Cache     => Cache,
                        Uncorrected_Finish_Point => [others => 0.0 * mm]);
                     Curve.Evaluator_Data.Uncorrected_Finish_Point :=
                       Evaluate_Uncorrected_Rational_Point (Curve.Evaluator_Data, 1.0);
                     --  The retained point formula is continuously anchored at both endpoints. Keep the exact check
                     --  as a construction invariant, but do not require a rounded scalar correction to close a
                     --  sub-ULP cancellation gap.
                     if Evaluate_Uncorrected_Rational_Point (Curve.Evaluator_Data, 1.0)
                       /= Curve.Evaluator_Data.Uncorrected_Finish_Point
                       or else Evaluate_Rational_Point (Curve.Evaluator_Data, 0.0) /= Curve.Evaluator_Data.Start_Point
                       or else Evaluate_Rational_Point (Curve.Evaluator_Data, 1.0) /= Curve.Evaluator_Data.Finish_Point
                     then
                        return Realtime_Representation_Insufficient;
                     end if;
                     Curve.Requested_Start_Jet := Start_Jet;
                     Curve.Requested_Finish_Jet := Finish_Jet;
                     Curve.Certified_Position_Error := Physical_Error;
                     Curve.Certified_Tangent_Error := Certificate.Error_Bound;
                     Curve.Evaluator_Evidence := Evidence;
                     --  Keep the common-denominator form if the Bernstein fallback built it, so the derivative
                     --  fallback need not rebuild it; it is invalid after the sampled proof alone. To_Evaluator
                     --  discards it because realtime point evaluation needs only the pole/residue representation.
                     Curve.Retained_Tangent_Certificate := Retained_Certificate;
                     return Realtime_Compilation_Succeeded;
                  end;
            end case;
         end;
      end;
   exception
      when Constraint_Error =>
         Curve.Evaluator_Data.Antiderivative_Cache := (others => <>);
         return Realtime_Compilation_Numerically_Unsafe;
   end Build_Rational_Representation;
   ----------------------------------------------------------------------------------------------------------------
   --  Candidate construction and public API
   ----------------------------------------------------------------------------------------------------------------

   ----------------------------------------------------------------------------------------------------------------
   --  Vector arithmetic of the geometric tests
   --
   --  Prunt's vector operators are outside SPARK, so GNATprove would take their results on trust. These do the same
   --  operations in the same order with stated limits: operands within the common limit give results below
   --  Vector_Wide_Limit, quotients of those by a divisor above 2⁻¹⁰⁰ stay below Vector_Quotient_Limit, and any two
   --  such vectors have a dot product below Vector_Dot_Limit. A caller tests a result against the common limit
   --  where it becomes an operand again.
   ----------------------------------------------------------------------------------------------------------------

   Vector_Wide_Limit     : constant := 2.0**250;
   Vector_Quotient_Limit : constant := 2.0**351;
   Vector_Dot_Limit      : constant := 2.0**704;

   function Next_Dot_Sum (Sum, Left, Right : Dimensionless; Terms_Before : Natural) return Dimensionless
   is (Sum + Left * Right)
   with
     Pre  =>
       Terms_Before <= 3
       and then abs Sum <= Dimensionless (Terms_Before) * 2.0**702
       and then abs Left <= Vector_Quotient_Limit
       and then abs Right <= Vector_Quotient_Limit,
     Post => abs Next_Dot_Sum'Result <= Dimensionless (Terms_Before + 1) * 2.0**702;
   pragma Inline (Next_Dot_Sum);

   function Vector_Dot (Left, Right : Position_Scale) return Dimensionless
   with
     Pre  => Within (Left, Vector_Quotient_Limit) and then Within (Right, Vector_Quotient_Limit),
     Post => abs Vector_Dot'Result <= Vector_Dot_Limit;
   --  The sum of the products of the components, one axis after another.

   function Vector_Dot (Left, Right : Position_Scale) return Dimensionless is
      Sum : Dimensionless := 0.0;
   begin
      for A in Axis_Name loop
         pragma Loop_Invariant (abs Sum <= Dimensionless (Axis_Name'Pos (A)) * 2.0**702);
         pragma Assert (abs Left (A) <= Vector_Quotient_Limit and then abs Right (A) <= Vector_Quotient_Limit);
         Sum := Next_Dot_Sum (Sum, Left (A), Right (A), Axis_Name'Pos (A));
      end loop;
      pragma Assert (abs Sum <= 4.0 * 2.0**702);
      return Sum;
   end Vector_Dot;

   function Next_Square_Sum (Sum, Value : Dimensionless; Terms_Before : Natural) return Dimensionless
   is (Sum + Value * Value)
   with
     Pre  =>
       Terms_Before <= 3
       and then Sum >= 0.0
       and then Sum <= Dimensionless (Terms_Before) * 2.0**702
       and then abs Value <= Vector_Quotient_Limit,
     Post =>
       Next_Square_Sum'Result >= 0.0 and then Next_Square_Sum'Result <= Dimensionless (Terms_Before + 1) * 2.0**702;
   pragma Inline (Next_Square_Sum);

   function Vector_Length (Value : Position_Scale) return Dimensionless
   with
     Pre  => Within (Value, Vector_Quotient_Limit),
     Post => Vector_Length'Result >= 0.0 and then Vector_Length'Result <= 2.0**352;
   --  The square root of a vector's dot product with itself.

   function Vector_Length (Value : Position_Scale) return Dimensionless is
      Sum : Dimensionless := 0.0;
   begin
      for A in Axis_Name loop
         pragma Loop_Invariant (Sum >= 0.0 and then Sum <= Dimensionless (Axis_Name'Pos (A)) * 2.0**702);
         pragma Assert (abs Value (A) <= Vector_Quotient_Limit);
         Sum := Next_Square_Sum (Sum, Value (A), Axis_Name'Pos (A));
      end loop;
      pragma Assert (Sum >= 0.0 and then Sum <= 4.0 * 2.0**702);
      Stereographic_Kernel.Elementary.Lemma_Sqrt_Is_At_Most (Sum, 2.0**352);
      return Dimensionless_Math.Sqrt (Sum);
   end Vector_Length;

   function Scaled (Value : Position_Scale; Factor : Dimensionless) return Position_Scale
   with
     Pre  => Within (Value, Tame_Limit) and then abs Factor <= Tame_Limit,
     Post => Within (Scaled'Result, Vector_Wide_Limit);
   --  Value times Factor.

   function Scaled (Value : Position_Scale; Factor : Dimensionless) return Position_Scale is
      Result : Position_Scale := [others => 0.0];
   begin
      for A in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => abs Result (B) <= Vector_Wide_Limit);
         pragma Assert (abs Value (A) <= Tame_Limit);
         declare
            Component : constant Dimensionless := Value (A) * Factor;
         begin
            pragma Assert (abs Component <= Vector_Wide_Limit);
            Result (A) := Component;
         end;
      end loop;
      return Result;
   end Scaled;

   function Minus_Scaled (Value, Direction : Position_Scale; Factor : Dimensionless) return Position_Scale
   with
     Pre  =>
       Within (Value, Vector_Wide_Limit) and then Within (Direction, Tame_Limit) and then abs Factor <= Tame_Limit,
     Post =>
       Within (Minus_Scaled'Result, 2.0 * Vector_Wide_Limit)
       and then (if Within (Value, Tame_Limit) then Within (Minus_Scaled'Result, Vector_Wide_Limit));
   --  Value less Factor times Direction.

   function Minus_Scaled (Value, Direction : Position_Scale; Factor : Dimensionless) return Position_Scale is
      Result : Position_Scale := [others => 0.0];
   begin
      for A in Axis_Name loop
         pragma
           Loop_Invariant
             (for all B in Axis_Name =>
                abs Result (B) <= 2.0 * Vector_Wide_Limit
                and then (if Within (Value, Tame_Limit) then abs Result (B) <= Vector_Wide_Limit));
         pragma Assert (abs Value (A) <= Vector_Wide_Limit and then abs Direction (A) <= Tame_Limit);
         pragma Assert (if Within (Value, Tame_Limit) then abs Value (A) <= Tame_Limit);
         declare
            Component : constant Dimensionless := Value (A) - Direction (A) * Factor;
         begin
            pragma Assert (abs Component <= 2.0 * Vector_Wide_Limit);
            pragma Assert (if abs Value (A) <= Tame_Limit then abs Component <= Vector_Wide_Limit);
            Result (A) := Component;
         end;
      end loop;
      return Result;
   end Minus_Scaled;

   function Divided (Value : Position_Scale; Divisor : Dimensionless) return Position_Scale
   with
     Pre  => Within (Value, 2.0 * Vector_Wide_Limit) and then abs Divisor >= 2.0**(-100),
     Post => Within (Divided'Result, Vector_Quotient_Limit);
   --  Value divided by Divisor.

   function Divided (Value : Position_Scale; Divisor : Dimensionless) return Position_Scale is
      Result : Position_Scale := [others => 0.0];
   begin
      for A in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => abs Result (B) <= Vector_Quotient_Limit);
         pragma Assert (abs Value (A) <= 2.0 * Vector_Wide_Limit);
         declare
            Component : constant Dimensionless := Value (A) / Divisor;
         begin
            pragma Assert (abs Component <= Vector_Quotient_Limit);
            Result (A) := Component;
         end;
      end loop;
      return Result;
   end Divided;

   procedure Lemma_Wide_Is_Quotient (Value : Position_Scale)
   with Ghost, Pre => Within (Value, 2.0 * Vector_Wide_Limit), Post => Within (Value, Vector_Quotient_Limit);

   procedure Lemma_Wide_Is_Quotient (Value : Position_Scale) is null;

   function Derivatives_Are_Moderate (Derivative : Tangent_Derivative_1_Vector) return Boolean
   is (for all Axis in Axis_Name => abs Derivative (Axis) <= Jet_Derivative_Limit / mm);

   function Turn_Rate (Derivative : Tangent_Derivative_1_Vector) return Position_Scale
   with Pre => Derivatives_Are_Moderate (Derivative), Post => Within (Turn_Rate'Result, Tame_Limit);
   --  The first tangent derivative in millimetres.

   function Turn_Rate (Derivative : Tangent_Derivative_1_Vector) return Position_Scale is
      Result : Position_Scale := [others => 0.0];
   begin
      for Axis in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => abs Result (B) <= Tame_Limit);
         pragma Assert (abs Derivative (Axis) <= Jet_Derivative_Limit / mm);
         declare
            Component : constant Dimensionless := Dimensionless (Derivative (Axis) * mm);
         begin
            pragma Assert (abs Component <= Tame_Limit);
            Result (Axis) := Component;
         end;
      end loop;
      return Result;
   end Turn_Rate;

   function Points_Are_Moderate (Request : Blend_Request) return Boolean
   is (for all Axis in Axis_Name =>
         abs Request.Start.Point (Axis) <= Coordinate_Limit
         and then abs Request.Finish.Point (Axis) <= Coordinate_Limit);
   --  Whether both endpoints are within the coordinate limit of the construction.

   Half_ULP_Limit : constant := 2.0**50;
   --  Above half the spacing of the floating-point numbers beside a coordinate or a chord of the construction.

   function Half_ULP (Value : Length) return Dimensionless
   with
     Pre  => abs Value <= 2.0 * Coordinate_Limit,
     Post => Half_ULP'Result >= 0.0 and then Half_ULP'Result <= Half_ULP_Limit;
   --  Return an outward-rounded half-width of the rounding cell represented by a stored coordinate, in
   --  millimetres. Both adjacent spacings are considered because floating-point bins can have different widths on
   --  either side of a value.

   function Half_ULP (Value : Length) return Dimensionless is
      Previous  : constant Length := (if Value > Length'First then Length'Pred (Value) else Value);
      Following : constant Length := (if Value < Length'Last then Length'Succ (Value) else Value);
      Below     : constant Dimensionless := abs Dimensionless ((Value - Previous) / mm);
      Above     : constant Dimensionless := abs Dimensionless ((Following - Value) / mm);
      --  The spacing beside a coordinate within the limit is below 2⁴⁹. Taking the smaller of the two states that
      --  for the proof, which knows the neighbours of a number only by their order.
      Spacing   : constant Dimensionless := Dimensionless'Min (2.0**49, Dimensionless'Max (Below, Above));
      Half      : constant Dimensionless := 0.5 * Spacing;
   begin
      --  Preserve a nonzero outward allowance at the subnormal boundary.
      return Dimensionless'Min (Half_ULP_Limit, (if Half = 0.0 and then Spacing > 0.0 then Spacing else Up (Half)));
   end Half_ULP;

   function Raw_Chord (Request : Blend_Request) return Position_Scale
   with Pre => Points_Are_Moderate (Request), Post => Within (Raw_Chord'Result, Tame_Limit);
   --  The chord between the endpoints in millimetres.

   function Raw_Chord (Request : Blend_Request) return Position_Scale is
      Result : Position_Scale := [others => 0.0];
   begin
      for Axis in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => abs Result (B) <= Tame_Limit);
         pragma
           Assert
             (abs Request.Start.Point (Axis) <= Coordinate_Limit
              and then abs Request.Finish.Point (Axis) <= Coordinate_Limit);
         declare
            Component : constant Dimensionless :=
              Dimensionless ((Request.Finish.Point (Axis) - Request.Start.Point (Axis)) / mm);
         begin
            pragma Assert (abs Component <= Tame_Limit);
            Result (Axis) := Component;
         end;
      end loop;
      return Result;
   end Raw_Chord;

   function Chord_Uncertainty_Of (Request : Blend_Request; Axis : Axis_Name) return Dimensionless
   with
     Pre  => Points_Are_Moderate (Request),
     Post => Chord_Uncertainty_Of'Result >= 0.0 and then Chord_Uncertainty_Of'Result <= 2.0**53;
   --  Treat each stored endpoint as the centre of its floating-point rounding cell: the half-widths of both cells
   --  and of the cell of their difference, on one axis.

   function Chord_Uncertainty_Of (Request : Blend_Request; Axis : Axis_Name) return Dimensionless is
   begin
      pragma
        Assert
          (abs Request.Start.Point (Axis) <= Coordinate_Limit
           and then abs Request.Finish.Point (Axis) <= Coordinate_Limit);
      declare
         Start_Half  : constant Dimensionless := Half_ULP (Request.Start.Point (Axis));
         Finish_Half : constant Dimensionless := Half_ULP (Request.Finish.Point (Axis));
         Chord_Half  : constant Dimensionless := Half_ULP (Request.Finish.Point (Axis) - Request.Start.Point (Axis));
         --  Each sum is far below the limit it is compared with, which states its size for the proof.
         Endpoints   : constant Dimensionless := Dimensionless'Min (2.0**52, Up (Start_Half + Finish_Half));
      begin
         return Dimensionless'Min (2.0**53, Up (Endpoints + Chord_Half));
      end;
   end Chord_Uncertainty_Of;

   function Chord_Uncertainties (Request : Blend_Request) return Position_Scale
   with
     Pre  => Points_Are_Moderate (Request),
     Post =>
       (for all Axis in Axis_Name =>
          Chord_Uncertainties'Result (Axis) >= 0.0 and then Chord_Uncertainties'Result (Axis) <= 2.0**53);

   function Chord_Uncertainties (Request : Blend_Request) return Position_Scale is
      Result : Position_Scale := [others => 0.0];
   begin
      for Axis in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => Result (B) >= 0.0 and then Result (B) <= 2.0**53);
         Result (Axis) := Chord_Uncertainty_Of (Request, Axis);
      end loop;
      return Result;
   end Chord_Uncertainties;

   function Projected_Term
     (Identity, Start_Out, Start_Dual_In, Finish_Out, Finish_Dual_In, Uncertainty : Dimensionless) return Dimensionless
   is (Up (abs (Identity - Start_Out * Start_Dual_In - Finish_Out * Finish_Dual_In) * Uncertainty))
   with
     Pre  =>
       abs Identity <= 1.0
       and then abs Start_Out <= Unit_Component_Limit
       and then abs Finish_Out <= Unit_Component_Limit
       and then abs Start_Dual_In <= Vector_Quotient_Limit
       and then abs Finish_Dual_In <= Vector_Quotient_Limit
       and then Uncertainty >= 0.0
       and then Uncertainty <= 2.0**53,
     Post => Projected_Term'Result >= 0.0;
   --  One input axis's uncertainty carried through the projector onto the complement of the endpoint plane.

   function Chord_Is_Numerically_In_Endpoint_Plane
     (Request : Blend_Request; Chord_Direction, Start_Tangent, Finish_Tangent : Position_Scale) return Boolean is
   begin
      --  The tangents are unit vectors and the chord direction is one too. Anything else, or an endpoint beyond the
      --  coordinate limit, is not a chord in a plane of tangents.
      if not Within (Start_Tangent, Unit_Component_Limit)
        or else not Within (Finish_Tangent, Unit_Component_Limit)
        or else not Within (Chord_Direction, Candidate_Component_Limit)
        or else not Points_Are_Moderate (Request)
      then
         return False;
      end if;
      Lemma_Within_Larger (Start_Tangent, Unit_Component_Limit, Tame_Limit);
      Lemma_Within_Larger (Finish_Tangent, Unit_Component_Limit, Tame_Limit);
      Lemma_Within_Larger (Chord_Direction, Candidate_Component_Limit, Tame_Limit);
      Lemma_Within_Larger (Start_Tangent, Tame_Limit, Vector_Quotient_Limit);
      Lemma_Within_Larger (Finish_Tangent, Tame_Limit, Vector_Quotient_Limit);
      Lemma_Within_Larger (Chord_Direction, Tame_Limit, Vector_Quotient_Limit);
      Lemma_Within_Larger (Start_Tangent, Tame_Limit, Vector_Wide_Limit);
      Lemma_Within_Larger (Finish_Tangent, Tame_Limit, Vector_Wide_Limit);
      Lemma_Within_Larger (Chord_Direction, Tame_Limit, Vector_Wide_Limit);
      declare
         --  The Gram determinant is the conditioning measure for resolving the chord in the two tangent
         --  directions. Nearly parallel tangents do not define a stable plane basis and must use the general
         --  closure path.
         G : constant Dimensionless := Vector_Dot (Start_Tangent, Finish_Tangent);
      begin
         if not (abs G <= Tame_Limit) then
            return False;
         end if;
         declare
            Denominator : constant Dimensionless := 1.0 - G * G;
         begin
            if not (Denominator > 1.0E-10) then
               return False;
            end if;
            declare
               Start_Dual  : constant Position_Scale :=
                 Divided (Minus_Scaled (Start_Tangent, Finish_Tangent, G), Denominator);
               Finish_Dual : constant Position_Scale :=
                 Divided (Minus_Scaled (Finish_Tangent, Start_Tangent, G), Denominator);
               Incoming    : constant Dimensionless := Vector_Dot (Chord_Direction, Start_Dual);
               Outgoing    : constant Dimensionless := Vector_Dot (Chord_Direction, Finish_Dual);
            begin
               if not (abs Incoming <= Tame_Limit) or else not (abs Outgoing <= Tame_Limit) then
                  return False;
               end if;
               declare
                  Scale                : constant Dimensionless :=
                    Dimensionless'Max (1.0, Dimensionless'Max (abs Incoming, abs Outgoing));
                  Residual             : constant Position_Scale :=
                    Minus_Scaled (Minus_Scaled (Chord_Direction, Start_Tangent, Incoming), Finish_Tangent, Outgoing);
                  Residual_OK          : Boolean;
                  Residual_Norm        : Dimensionless;
                  Arithmetic_Tolerance : constant Dimensionless := 4_096.0 * Dimensionless'Model_Epsilon * Scale;
               begin
                  Residual_Norm := Safe_Norm (Residual, Residual_OK);
                  if not Residual_OK then
                     return False;
                  elsif Residual_Norm <= Arithmetic_Tolerance then
                     --  A residual covered by normalized arithmetic error is accepted directly; only a larger one
                     --  is compared with the projected rounding cells of the stored coordinates.
                     return True;
                  end if;

                  declare
                     Chord_Raw    : constant Position_Scale := Raw_Chord (Request);
                     Raw_Incoming : Dimensionless;
                     Raw_Outgoing : Dimensionless;
                  begin
                     Lemma_Within_Larger (Chord_Raw, Tame_Limit, Vector_Quotient_Limit);
                     Lemma_Within_Larger (Chord_Raw, Tame_Limit, Vector_Wide_Limit);
                     Raw_Incoming := Vector_Dot (Chord_Raw, Start_Dual);
                     Raw_Outgoing := Vector_Dot (Chord_Raw, Finish_Dual);
                     if not (abs Raw_Incoming <= Tame_Limit) or else not (abs Raw_Outgoing <= Tame_Limit) then
                        return False;
                     end if;
                     declare
                        Raw_Residual      : constant Position_Scale :=
                          Minus_Scaled
                            (Minus_Scaled (Chord_Raw, Start_Tangent, Raw_Incoming), Finish_Tangent, Raw_Outgoing);
                        Largest           : constant Dimensionless :=
                          Dimensionless'Max
                            (abs Chord_Raw (X_Axis),
                             Dimensionless'Max
                               (abs Chord_Raw (Y_Axis),
                                Dimensionless'Max (abs Chord_Raw (Z_Axis), abs Chord_Raw (E_Axis))));
                        Chord_Uncertainty : constant Position_Scale := Chord_Uncertainties (Request);
                        Arithmetic_Radius : Dimensionless;
                     begin
                        pragma Assert (for all Axis in Axis_Name => abs Chord_Raw (Axis) <= Tame_Limit);
                        pragma Assert (Largest >= 0.0 and then Largest <= Tame_Limit);
                        pragma Assert (Arithmetic_Tolerance >= 0.0 and then Arithmetic_Tolerance <= Tame_Limit);
                        Arithmetic_Radius := Up (Arithmetic_Tolerance * Largest);
                        if not (Arithmetic_Radius <= Tame_Limit) then
                           return False;
                        end if;

                        --  Project each axis's physical uncertainty through the same orthogonal complement used
                        --  to measure the endpoint-plane residual. This admits accumulated-coordinate cancellation
                        --  independently of chord length without allowing the ULP of an unrelated axis to excuse a
                        --  real normal displacement.
                        for Output_Axis in Axis_Name loop
                           declare
                              Projected_Uncertainty : Dimensionless := 0.0;
                           begin
                              for Input_Axis in Axis_Name loop
                                 pragma
                                   Loop_Invariant
                                     (Projected_Uncertainty >= 0.0 and then Projected_Uncertainty <= Tame_Limit);
                                 pragma
                                   Assert
                                     (abs Start_Tangent (Output_Axis) <= Unit_Component_Limit
                                      and then abs Finish_Tangent (Output_Axis) <= Unit_Component_Limit);
                                 pragma
                                   Assert
                                     (abs Start_Dual (Input_Axis) <= Vector_Quotient_Limit
                                      and then abs Finish_Dual (Input_Axis) <= Vector_Quotient_Limit);
                                 pragma
                                   Assert
                                     (Chord_Uncertainty (Input_Axis) >= 0.0
                                      and then Chord_Uncertainty (Input_Axis) <= 2.0**53);
                                 declare
                                    Term : constant Dimensionless :=
                                      Projected_Term
                                        ((if Output_Axis = Input_Axis then 1.0 else 0.0),
                                         Start_Tangent (Output_Axis),
                                         Start_Dual (Input_Axis),
                                         Finish_Tangent (Output_Axis),
                                         Finish_Dual (Input_Axis),
                                         Chord_Uncertainty (Input_Axis));
                                 begin
                                    --  An allowance beyond the common limit excuses nothing.
                                    if not (Term <= Tame_Limit) then
                                       return False;
                                    end if;
                                    Projected_Uncertainty := Up (Projected_Uncertainty + Term);
                                    if not (Projected_Uncertainty <= Tame_Limit) then
                                       return False;
                                    end if;
                                 end;
                              end loop;
                              if abs Raw_Residual (Output_Axis) > Up (Projected_Uncertainty + Arithmetic_Radius) then
                                 return False;
                              end if;
                           end;
                        end loop;
                        return True;
                     end;
                  end;
               end;
            end;
         end;
      end;
   end Chord_Is_Numerically_In_Endpoint_Plane;

   function Next_Work (Work, Term : Dimensionless; Slots_Before : Natural) return Dimensionless
   is (Work + Term)
   with
     Pre  =>
       Slots_Before <= Maximum_Rational_Degree
       and then Work >= 0.0
       and then Work <= Dimensionless (Slots_Before + 1) * Slot_Term_Limit
       and then Term >= 0.0
       and then Term <= Slot_Term_Limit,
     Post => Next_Work'Result >= 0.0 and then Next_Work'Result <= Dimensionless (Slots_Before + 2) * Slot_Term_Limit;
   --  The work of an evaluation with one more pole or pair, as Add_Real_Pole bounds the displacement.

   function Pair_Work (Log_Work, Real_Residue, Imaginary_Residue : Dimensionless) return Dimensionless
   is (Log_Work * abs Real_Residue + 2.0 * Dimensionless (Ada.Numerics.Pi) * abs Imaginary_Residue)
   with
     Pre  =>
       Log_Work >= 0.0
       and then Log_Work <= Log_Ratio_Limit
       and then abs Real_Residue <= Residue_Magnitude_Limit
       and then abs Imaginary_Residue <= Residue_Magnitude_Limit,
     Post => Pair_Work'Result >= 0.0 and then Pair_Work'Result <= Slot_Term_Limit;

   function Normalized_Evaluation_Work (Cache : Rational_Antiderivative; Axis : Axis_Name) return Dimensionless
   with
     Pre  => Rational_Antiderivative_Is_Well_Formed (Cache),
     Post => Normalized_Evaluation_Work'Result >= 0.0 and then Normalized_Evaluation_Work'Result <= Displacement_Limit;
   --  The sum of the magnitudes of the terms which an evaluation at the finish adds on one axis, and at least one.

   function Normalized_Evaluation_Work (Cache : Rational_Antiderivative; Axis : Axis_Name) return Dimensionless is
      Work : Dimensionless;
   begin
      pragma Assert (abs Cache.Constant_Tangent (Axis) <= Residue_Magnitude_Limit);
      Work := Dimensionless'Max (1.0, abs Cache.Constant_Tangent (Axis));
      for Index in 1 .. Cache.Real_Pole_Count loop
         pragma Loop_Invariant (Work >= 0.0 and then Work <= Dimensionless (Index) * Slot_Term_Limit);
         pragma Assert (Slot_Is_Tame (Cache.Pole_Slots (Index)));
         pragma Assert (Is_Clear_Of_Unit_Interval (Cache.Pole_Slots (Index).Pole_Component));
         pragma Assert (abs Cache.Pole_Slots (Index).Residue_Component (Axis) <= Residue_Magnitude_Limit);
         declare
            Primitive : constant Dimensionless :=
              abs Stable_Real_Log_Ratio (Cache.Pole_Slots (Index).Pole_Component, 1.0);
            Residue   : constant Dimensionless := abs Cache.Pole_Slots (Index).Residue_Component (Axis);
            Term      : constant Dimensionless := Primitive * Residue;
         begin
            pragma Assert (Term >= 0.0 and then Term <= Slot_Term_Limit);
            Work := Next_Work (Work, Term, Index - 1);
         end;
      end loop;
      pragma Assert (Work >= 0.0 and then Work <= Dimensionless (Cache.Real_Pole_Count + 1) * Slot_Term_Limit);
      for Pair in 1 .. Cache.Pair_Count loop
         pragma
           Loop_Invariant
             (Work >= 0.0 and then Work <= Dimensionless (Cache.Real_Pole_Count + Pair) * Slot_Term_Limit);
         pragma Assert (Slot_Is_Tame (Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair - 1)));
         pragma Assert (Slot_Is_Tame (Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair)));
         pragma
           Assert
             (Pair_Is_Clear
                (Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair - 1).Pole_Component,
                 Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair).Pole_Component));
         declare
            Real_Slot      : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair)));
            Imaginary_Slot : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair)));
            A              : constant Dimensionless := Real_Slot.Pole_Component;
            B              : constant Dimensionless := Imaginary_Slot.Pole_Component;
            Closest        : constant Dimensionless := Dimensionless'Max (0.0, Dimensionless'Min (1.0, A));
            Closest_Log    : constant Dimensionless := Stable_Complex_Log_Ratio (A, B, Closest);
            Finish_Log     : constant Dimensionless := Stable_Complex_Log_Ratio (A, B, 1.0);
            Log_Work       : constant Dimensionless := Dimensionless'Max (abs Closest_Log, abs Finish_Log);
         begin
            pragma Assert (abs Real_Slot.Residue_Component (Axis) <= Residue_Magnitude_Limit);
            pragma Assert (abs Imaginary_Slot.Residue_Component (Axis) <= Residue_Magnitude_Limit);
            Work :=
              Next_Work
                (Work,
                 Pair_Work (Log_Work, Real_Slot.Residue_Component (Axis), Imaginary_Slot.Residue_Component (Axis)),
                 Cache.Real_Pole_Count + Pair - 1);
         end;
      end loop;
      pragma
        Assert
          (Work >= 0.0
           and then Work <= Dimensionless (Cache.Real_Pole_Count + Cache.Pair_Count + 1) * Slot_Term_Limit);
      pragma Assert (Dimensionless (Cache.Real_Pole_Count + Cache.Pair_Count + 1) <= 17.0);
      return Work;
   end Normalized_Evaluation_Work;

   Roundoff_Error_Limit : constant := 2.0**400;
   --  Largest roundoff bound of one axis which Realtime_Point_Roundoff_Bound combines.

   function Axis_Roundoff (Evaluator : Stereographic_Curve_Evaluator; Axis : Axis_Name) return Dimensionless
   with
     Pre  => Evaluator.Kind = Positive_Curve_Kind,
     Post => Axis_Roundoff'Result >= 0.0 and then Axis_Roundoff'Result <= Roundoff_Error_Limit;
   --  A bound on the rounding of one coordinate of an evaluated point, in millimetres: the transcendental
   --  primitive work, the dimensional multiplication and the translated coordinate addition.

   function Axis_Roundoff (Evaluator : Stereographic_Curve_Evaluator; Axis : Axis_Name) return Dimensionless is
      Normalized_Work : constant Dimensionless := Normalized_Evaluation_Work (Evaluator.Antiderivative_Cache, Axis);
      Coordinate_Work : Dimensionless;
   begin
      pragma
        Assert
          (abs Evaluator.Start_Point (Axis) <= Coordinate_Limit
           and then abs Evaluator.Finish_Point (Axis) <= Coordinate_Limit
           and then abs Evaluator.Uncorrected_Finish_Point (Axis) <= Coordinate_Limit);
      Coordinate_Work :=
        Dimensionless'Max
          (1.0,
           Dimensionless'Max
             (abs Dimensionless (Evaluator.Start_Point (Axis) / mm),
              abs Dimensionless (Evaluator.Finish_Point (Axis) / mm)));
      Coordinate_Work :=
        Dimensionless'Max (Coordinate_Work, abs Dimensionless (Evaluator.Length_Value / mm) * (1.0 + Normalized_Work));
      Coordinate_Work :=
        Dimensionless'Max (Coordinate_Work, abs Dimensionless (Evaluator.Uncorrected_Finish_Point (Axis) / mm));
      pragma Assert (Coordinate_Work >= 0.0 and then Coordinate_Work <= 2.0**222);
      --  The bound is far below the limit it is compared with, which states its size for the proof.
      return Dimensionless'Min (Roundoff_Error_Limit, Up (16_384.0 * Dimensionless'Model_Epsilon * Coordinate_Work));
   end Axis_Roundoff;

   function Realtime_Point_Roundoff_Bound (Evaluator : Stereographic_Curve_Evaluator) return Length
   with Pre => Evaluator.Kind = Positive_Curve_Kind, Post => Realtime_Point_Roundoff_Bound'Result >= 0.0 * mm;
   --  Bound floating-point roundoff in public point evaluation directly from the executable evaluator.

   function Realtime_Point_Roundoff_Bound (Evaluator : Stereographic_Curve_Evaluator) return Length is
      Axis_Errors : Dimensionless_Axis_Vector := [others => 0.0];
      Scale       : Dimensionless := 0.0;
      Sum         : Dimensionless := 0.0;
   begin
      --  The rational certificate is expressed as normalized displacement from Start_Point. Include the
      --  transcendental primitive work, dimensional multiplication, and translated coordinate addition which public
      --  evaluation performs afterward.
      for Axis in Axis_Name loop
         pragma
           Loop_Invariant
             (Scale >= 0.0
              and then Scale <= Roundoff_Error_Limit
              and then (for all B in Axis_Name => Axis_Errors (B) >= 0.0 and then Axis_Errors (B) <= Scale));
         declare
            Error : constant Dimensionless := Axis_Roundoff (Evaluator, Axis);
         begin
            Axis_Errors (Axis) := Error;
            Scale := Dimensionless'Max (Scale, Error);
         end;
      end loop;
      pragma Assert (for all B in Axis_Name => Axis_Errors (B) >= 0.0 and then Axis_Errors (B) <= Scale);
      if Scale = 0.0 then
         return 0.0 * mm;
      end if;
      --  Combine per-axis error bounds with a scaled Euclidean norm to avoid squaring large absolute coordinates.
      for Axis in Axis_Name loop
         pragma Loop_Invariant (Sum >= 0.0 and then Sum <= Tame_Limit);
         pragma Assert (Axis_Errors (Axis) >= 0.0 and then Axis_Errors (Axis) <= Scale);
         declare
            Ratio : constant Dimensionless := Axis_Errors (Axis) / Scale;
         begin
            pragma Assert (Ratio >= 0.0 and then Ratio <= 1.0);
            Sum := Up (Sum + Ratio * Ratio);
            --  The sum of four squares at most one is at most a little over four. Anything else would be no bound.
            if not (Sum <= Tame_Limit) then
               return Length'Last;
            end if;
         end;
      end loop;
      declare
         Unit_Norm_Bound : constant Dimensionless := Certified_Upper_Square_Root (Sum);
      begin
         if not (Unit_Norm_Bound <= Tame_Limit) then
            return Length'Last;
         end if;
         return Up (Scale * Unit_Norm_Bound) * mm;
      end;
   end Realtime_Point_Roundoff_Bound;

   function No_Bulge_Is_Possible (Request : Blend_Request; Chord_Direction : Position_Scale) return Boolean;
   --  Check first-order conditions which every curve inside the endpoint cone satisfies: the cone has not collapsed,
   --  the chord lies inside it, and each endpoint curvature turns the tangent into it. A False result proves that the
   --  no-bulge certificate cannot succeed; a True result leaves the decision to that certificate.

   function No_Bulge_Is_Possible (Request : Blend_Request; Chord_Direction : Position_Scale) return Boolean is
      Start_Tangent   : Position_Scale renames Request.Start.Jet.Tangent;
      Finish_Tangent  : Position_Scale renames Request.Finish.Jet.Tangent;
      Turn_Tolerance  : constant Dimensionless := 1.0E-9;
      Chord_Tolerance : constant Dimensionless := 1.0E-6;
      --  Only clear violations count. The chord allowance exceeds every closure tolerance, because the endpoint
      --  correction absorbs the closure residual of an ideal curve which is itself inside the cone.
   begin
      --  The tangents and the chord direction are unit vectors, and a curvature beyond the limit is none of a
      --  machine's. Anything else has no cone to stay in.
      if not Within (Start_Tangent, Unit_Component_Limit)
        or else not Within (Finish_Tangent, Unit_Component_Limit)
        or else not Within (Chord_Direction, Candidate_Component_Limit)
        or else not Derivatives_Are_Moderate (Request.Start.Jet.Tangent_Derivative_1)
        or else not Derivatives_Are_Moderate (Request.Finish.Jet.Tangent_Derivative_1)
      then
         return False;
      end if;
      Lemma_Within_Larger (Start_Tangent, Unit_Component_Limit, Tame_Limit);
      Lemma_Within_Larger (Finish_Tangent, Unit_Component_Limit, Tame_Limit);
      Lemma_Within_Larger (Chord_Direction, Candidate_Component_Limit, Tame_Limit);
      Lemma_Within_Larger (Start_Tangent, Tame_Limit, Vector_Quotient_Limit);
      Lemma_Within_Larger (Finish_Tangent, Tame_Limit, Vector_Quotient_Limit);
      Lemma_Within_Larger (Chord_Direction, Tame_Limit, Vector_Quotient_Limit);
      Lemma_Within_Larger (Chord_Direction, Tame_Limit, Vector_Wide_Limit);
      declare
         Start_Turn_Rate  : constant Position_Scale := Turn_Rate (Request.Start.Jet.Tangent_Derivative_1);
         Finish_Turn_Rate : constant Position_Scale := Turn_Rate (Request.Finish.Jet.Tangent_Derivative_1);
         Start_Square     : constant Dimensionless := Vector_Dot (Start_Tangent, Start_Tangent);
         Finish_Square    : constant Dimensionless := Vector_Dot (Finish_Tangent, Finish_Tangent);
         Cross            : constant Dimensionless := Vector_Dot (Start_Tangent, Finish_Tangent);
      begin
         if not (abs Start_Square <= Tame_Limit)
           or else not (abs Finish_Square <= Tame_Limit)
           or else not (abs Cross <= Tame_Limit)
         then
            return False;
         end if;
         Lemma_Within_Larger (Start_Turn_Rate, Tame_Limit, Vector_Quotient_Limit);
         Lemma_Within_Larger (Finish_Turn_Rate, Tame_Limit, Vector_Quotient_Limit);
         declare
            Determinant      : constant Dimensionless := Start_Square * Finish_Square - Cross * Cross;
            --  In the endpoint plane, Start_Dual is orthogonal to the finish tangent and Finish_Dual to the start
            --  tangent. A tangent a·s + b·f + (normal part) projects onto them as a·Δ and b·Δ, where Δ is the Gram
            --  determinant.
            Start_Dual       : constant Position_Scale :=
              Minus_Scaled (Scaled (Start_Tangent, Finish_Square), Finish_Tangent, Cross);
            Finish_Dual      : constant Position_Scale :=
              Minus_Scaled (Scaled (Finish_Tangent, Start_Square), Start_Tangent, Cross);
            Start_Dual_Norm  : Dimensionless;
            Finish_Dual_Norm : Dimensionless;
         begin
            Lemma_Wide_Is_Quotient (Start_Dual);
            Lemma_Wide_Is_Quotient (Finish_Dual);
            Start_Dual_Norm := Vector_Length (Start_Dual);
            Finish_Dual_Norm := Vector_Length (Finish_Dual);
            if not (Determinant > 256.0 * Dimensionless'Model_Epsilon * Start_Square * Finish_Square) then
               --  A collapsed cone admits only a straight curve along the shared tangent, which the planar
               --  control-hull certificate accepts. Anything leaving that line is impossible.
               if not (Start_Square >= 0.25) then
                  return False;
               end if;
               declare
                  Along : constant Dimensionless := Vector_Dot (Chord_Direction, Start_Tangent) / Start_Square;
               begin
                  if not (abs Along <= Tame_Limit) then
                     return False;
                  end if;
                  declare
                     Off : constant Position_Scale := Minus_Scaled (Chord_Direction, Start_Tangent, Along);
                  begin
                     Lemma_Wide_Is_Quotient (Off);
                     return
                       Cross > 0.0
                       and then Along > 0.0
                       and then Vector_Dot (Off, Off) <= Chord_Tolerance * Chord_Tolerance;
                  end;
               end;
            end if;
            --  Near the start the tangent is s + u·κₛ, whose finish-dual projection u·κₛ·Finish_Dual must not be
            --  negative. Near the finish it is f - u·κꜰ, whose start-dual projection -u·κꜰ·Start_Dual must not be
            --  negative either.
            return
              Vector_Dot (Chord_Direction, Start_Dual) >= -Chord_Tolerance * Start_Dual_Norm
              and then Vector_Dot (Chord_Direction, Finish_Dual) >= -Chord_Tolerance * Finish_Dual_Norm
              and then
                Vector_Dot (Start_Turn_Rate, Finish_Dual)
                >= -Turn_Tolerance * Vector_Length (Start_Turn_Rate) * Finish_Dual_Norm
              and then
                Vector_Dot (Finish_Turn_Rate, Start_Dual)
                <= Turn_Tolerance * Vector_Length (Finish_Turn_Rate) * Start_Dual_Norm;
         end;
      end;
   end No_Bulge_Is_Possible;

   procedure Lemma_Chart_Vector_Within_Larger (Value : Chart_Vector; Smaller, Larger : Dimensionless)
   with Ghost, Pre => Within (Value, Smaller) and then Smaller <= Larger, Post => Within (Value, Larger);

   procedure Lemma_Chart_Vector_Within_Larger (Value : Chart_Vector; Smaller, Larger : Dimensionless) is null;

   procedure Lemma_Vector_4_Within_Larger (Value : Raw_Vector_4; Smaller, Larger : Dimensionless)
   with Ghost, Pre => Within (Value, Smaller) and then Smaller <= Larger, Post => Within (Value, Larger);

   procedure Lemma_Vector_4_Within_Larger (Value : Raw_Vector_4; Smaller, Larger : Dimensionless) is null;

   procedure Clear_Chart_Coefficient
     (Chart : in out Bernstein_Chart; Index : Chart_Coefficient_Index; Component : Chart_Component_Index)
   with Pre => Within (Chart, Tame_Limit), Post => Within (Chart, Tame_Limit);

   procedure Clear_Chart_Coefficient
     (Chart : in out Bernstein_Chart; Index : Chart_Coefficient_Index; Component : Chart_Component_Index) is
   begin
      Chart (Index, Component) := 0.0;
   end Clear_Chart_Coefficient;

   function Chart_Tangent (Frame : Frame_Vector_Array; Point : Chart_Vector) return Position_Scale
   with Pre => Within (Frame, Unit_Component_Limit) and then Within (Point, Tame_Limit);
   --  The tangent on the machine axes of a chart point.

   function Chart_Tangent (Frame : Frame_Vector_Array; Point : Chart_Vector) return Position_Scale is
   begin
      Lemma_Chart_Vector_Within_Larger (Point, Tame_Limit, Chart_Point_Limit);
      declare
         Sphere : constant Raw_Vector_4 := Stereographic (Point);
      begin
         Lemma_Vector_4_Within_Larger (Sphere, 2.0 * Chart_Point_Limit, Frame_Operand_Limit);
         return Frame_Vector (Frame, Sphere);
      end;
   end Chart_Tangent;

   procedure Clear_Closure_Coefficient (C0 : in out Raw_Vector_3; Component : Chart_Component_Index)
   with Pre => Within (C0, Tame_Limit), Post => Within (C0, Tame_Limit);

   procedure Clear_Closure_Coefficient (C0 : in out Raw_Vector_3; Component : Chart_Component_Index) is
   begin
      C0 (Component) := 0.0;
   end Clear_Closure_Coefficient;

   procedure Remove_Normal_Noise
     (Request                             : Blend_Request;
      Chord_Direction                     : Position_Scale;
      Frame                               : Frame_Vector_Array;
      Start_Chart_Jets, Finish_Chart_Jets : Chart_Jet_Array;
      Bulge_Allowed                       : Boolean;
      Coefficients                        : in out Bernstein_Chart;
      Effective_C0                        : in out Raw_Vector_3)
   with
     Pre  =>
       Within (Chord_Direction, Candidate_Component_Limit)
       and then Within (Frame, Unit_Component_Limit)
       and then Within (Start_Chart_Jets, Tame_Limit)
       and then Within (Finish_Chart_Jets, Tame_Limit)
       and then Within (Coefficients, Tame_Limit)
       and then Within (Effective_C0, Tame_Limit),
     Post => Within (Coefficients, Tame_Limit) and then Within (Effective_C0, Tame_Limit);
   --  The no-bulge certificates treat the last two chart coordinates as normal components. For flat endpoint jets
   --  and a chord in the endpoint-tangent plane up to stored-coordinate resolution, cancellation can leave
   --  roundoff-sized residues there, and the closure solve can amplify them into a spurious normal mode.
   --  Canonicalize the structurally planar result before deriving any runtime representation. When bulging is not
   --  allowed, the normal closure mode is removed outright: flat in-plane endpoint jets and an in-plane chord give
   --  it no source. Otherwise it is removed only when every coefficient proves that it is numerical noise. The
   --  representation and continuous endpoint-correction certificates still gate the resulting production curve.

   procedure Remove_Normal_Noise
     (Request                             : Blend_Request;
      Chord_Direction                     : Position_Scale;
      Frame                               : Frame_Vector_Array;
      Start_Chart_Jets, Finish_Chart_Jets : Chart_Jet_Array;
      Bulge_Allowed                       : Boolean;
      Coefficients                        : in out Bernstein_Chart;
      Effective_C0                        : in out Raw_Vector_3) is
   begin
      pragma Assert (Within (Start_Chart_Jets (0), Tame_Limit) and then Within (Finish_Chart_Jets (0), Tame_Limit));
      declare
         Endpoint_Derivatives_Are_Flat : constant Boolean :=
           (for all K in 1 .. Start_Chart_Jets'Last =>
              (for all C in Chart_Component_Index =>
                 Start_Chart_Jets (K) (C) = 0.0 and then Finish_Chart_Jets (K) (C) = 0.0));
         Start_Tangent                 : constant Position_Scale := Chart_Tangent (Frame, Start_Chart_Jets (0));
         Finish_Tangent                : constant Position_Scale := Chart_Tangent (Frame, Finish_Chart_Jets (0));
         Chord_Is_Structurally_Planar  : constant Boolean :=
           Chord_Is_Numerically_In_Endpoint_Plane (Request, Chord_Direction, Start_Tangent, Finish_Tangent);
      begin
         Lemma_Within_Larger (Chord_Direction, Candidate_Component_Limit, Frame_Operand_Limit);
         if Endpoint_Derivatives_Are_Flat then
            for C in 2 .. 3 loop
               pragma Loop_Invariant (Within (Coefficients, Tame_Limit));
               pragma Loop_Invariant (Within (Effective_C0, Tame_Limit));
               pragma Assert (Within (Frame (C), Unit_Component_Limit));
               if abs Start_Chart_Jets (0) (C) <= Frame_Residual_Tolerance
                 and then abs Finish_Chart_Jets (0) (C) <= Frame_Residual_Tolerance
                 and then
                   (abs Frame_Coordinate (Frame, C, Chord_Direction) <= Frame_Residual_Tolerance
                    or else Chord_Is_Structurally_Planar)
                 and then
                   (not Bulge_Allowed
                    or else
                      (for all K in Chart_Coefficient_Index => abs Coefficients (K, C) <= Frame_Residual_Tolerance))
               then
                  for K in Chart_Coefficient_Index loop
                     pragma Loop_Invariant (Within (Coefficients, Tame_Limit));
                     Clear_Chart_Coefficient (Coefficients, K, C);
                  end loop;
                  Clear_Closure_Coefficient (Effective_C0, C);
               end if;
            end loop;
         end if;

         --  A frame built from the endpoint-tangent plane leaves only roundoff in the endpoint normal coordinates.
         --  Remove it so the spatial no-bulge certificate sees both endpoint tangents exactly in the frame plane.
         for C in 2 .. 3 loop
            pragma Loop_Invariant (Within (Coefficients, Tame_Limit));
            if abs Coefficients (0, C) <= Frame_Residual_Tolerance then
               Clear_Chart_Coefficient (Coefficients, 0, C);
            end if;
            if abs Coefficients (Fixed_Chart_Degree, C) <= Frame_Residual_Tolerance then
               Clear_Chart_Coefficient (Coefficients, Fixed_Chart_Degree, C);
            end if;
         end loop;
      end;
   end Remove_Normal_Noise;

   function Failed_Ideal_Candidate (Status : Candidate_Status) return Ideal_Candidate
   is ((Status => Status, others => <>))
   with
     Pre  => Status /= Candidate_Success,
     Post =>
       Failed_Ideal_Candidate'Result.Status = Status
       and then Within (Failed_Ideal_Candidate'Result.Coefficients, Tame_Limit)
       and then Within (Failed_Ideal_Candidate'Result.Effective_C0, Tame_Limit);

   function Build_Ideal_Candidate
     (Request                             : Blend_Request;
      Chord_Length                        : Length;
      Chord_Direction                     : Position_Scale;
      Frame                               : Frame_Vector_Array;
      Start_Chart_Jets, Finish_Chart_Jets : Chart_Jet_Array;
      Warp_Factor                         : Dimensionless;
      Initial_Closure                     : Closure_Seed) return Ideal_Candidate
   is
      --  The closure solve considers no curve longer than Lambda_Limit times its chord, whatever the request allows.
      Maximum_Lambda   : constant Dimensionless :=
        Dimensionless'Min (Lambda_Limit, Dimensionless (Request.Maximum_Arc_Length / Chord_Length));
      Epsilon          : constant Dimensionless := Dimensionless (Request.Maximum_Position_Error / Chord_Length);
      --  Closure is solved in chord-normalized coordinates. Tie both Newton tolerances to the available normalized
      --  position budget ε = Maximum_Position_Error/Chord_Length, while retaining an arithmetic noise floor.
      Coarse_Tolerance : constant Dimensionless :=
        Dimensionless'Max (Closure_Absolute_Floor, Dimensionless'Min (1.0E-8, Epsilon / 2_048.0));
      Fine_Tolerance   : constant Dimensionless :=
        Dimensionless'Max (Closure_Absolute_Floor, Dimensionless'Min (1.0E-11, Epsilon / 4_096.0));
      Bulge_Allowed    : constant Boolean :=
        (case Request.Bulge is
           when Never            => False,
           when When_Unavoidable => not No_Bulge_Is_Possible (Request, Chord_Direction),
           when Always           => True);

      Solution : Closure_Solution := (Success => False, Numerically_Unsafe => True, others => <>);
   begin
      if Initial_Closure.Valid then
         --  Select_Distance_Warp integrates the endpoint-flat chart in its natural coordinate, where even a strong
         --  Möbius map remains well resolved. Re-solving that proposal with a coarse grid in U can move a good
         --  asymmetric solution away from closure. The final global tangent certificate and continuous endpoint
         --  correction still verify the proposal. If either fails, the candidate fails; there is no unwarped or
         --  unseeded fallback.
         Solution :=
           (Success => True, Numerically_Unsafe => False, Lambda => Initial_Closure.Lambda, C0 => Initial_Closure.C0);
      else
         --  Solve closure on a coarse grid and refine the result on a finer one.
         declare
            Terms : constant Chart_Lambda_Terms :=
              Build_Chart_Lambda_Terms (Start_Chart_Jets, Finish_Chart_Jets, Warp_Factor);
         begin
            --  The solve takes the terms of the chart within the common limit, as every chart worth solving has
            --  them. Otherwise the candidate is numerically unsafe.
            if Within (Terms, Tame_Limit) then
               Solution :=
                 Solve_Closure_From_Terms
                   (Terms,
                    Frame,
                    Chord_Direction,
                    Maximum_Lambda,
                    Coarse_Tolerance,
                    Warp_Factor,
                    Initial     => (others => <>),
                    Panel_Count => 1);
               if Solution.Success then
                  Solution :=
                    Solve_Closure_From_Terms
                      (Terms,
                       Frame,
                       Chord_Direction,
                       Maximum_Lambda,
                       Fine_Tolerance,
                       Warp_Factor,
                       Initial     => Solution,
                       Panel_Count => 2);
               end if;
            end if;
         end;
      end if;

      if not Solution.Success then
         return
           Failed_Ideal_Candidate
             (if Solution.Numerically_Unsafe then Candidate_Numerically_Unsafe else Candidate_Closure_Failed);
      end if;

      declare
         Coefficients     : Bernstein_Chart;
         Effective_C0     : Raw_Vector_3 := Solution.C0;
         --  λ = Arc_Length/Chord_Length. Clamp the solved value back into its analytic and requested bounds before
         --  rebuilding the authoritative chart.
         Length_Value     : constant Length :=
           Length'Max (Chord_Length, Length'Min (Request.Maximum_Arc_Length, Solution.Lambda * Chord_Length));
         Effective_Lambda : constant Dimensionless :=
           Dimensionless'Max (1.0, Dimensionless'Min (Lambda_Limit, Dimensionless (Length_Value / Chord_Length)));
      begin
         pragma Assert (Within (Effective_C0, Tame_Limit));
         Build_Chart (Start_Chart_Jets, Finish_Chart_Jets, Effective_Lambda, Effective_C0, Warp_Factor, Coefficients);
         --  The stages which follow take the chart within the common limit.
         if not Within (Coefficients, Tame_Limit) then
            return Failed_Ideal_Candidate (Candidate_Numerically_Unsafe);
         end if;
         Remove_Normal_Noise
           (Request,
            Chord_Direction,
            Frame,
            Start_Chart_Jets,
            Finish_Chart_Jets,
            Bulge_Allowed,
            Coefficients,
            Effective_C0);
         return
           (Status        => Candidate_Success,
            Coefficients  => Coefficients,
            Effective_C0  => Effective_C0,
            Length_Value  => Length_Value,
            Bulge_Allowed => Bulge_Allowed);
      end;
   end Build_Ideal_Candidate;

   function Strict_Bernstein_Nonnegative (Coefficients : Interval_Polynomial; Depth : Natural) return Boolean is
      --  A nonnegative lower bound on every control certifies p(U) ≥ 0 by the Bernstein convex-hull property. If
      --  the full hull straddles zero, bisect with de Casteljau and retry each half.
      --
      --  Bisection only averages neighbouring controls, and an average increases with each of its two terms.
      --  The lower bounds of the controls of a half are therefore the averages of lower bounds, and likewise
      --  for the upper bounds, so the two sets are carried as plain arrays rather than as intervals. One
      --  rounded sum is correct to half a unit in its last place and halving it is exact, or errs by less than
      --  Model_Small for a subnormal; Inflate_Finite moves each result outward by more than either.
      Degree          : constant Natural := Coefficients'Length - 1;
      Magnitude_Limit : constant Dimensionless := Dimensionless'Last / 8.0;
      --  Averages never exceed the larger of their terms, and the outward padding compounds by less than a
      --  factor of two over every level, so no value below this limit can leave Inflate_Finite's range.

      type Control_Bounds is array (0 .. Degree) of Dimensionless;

      function Decide (Lower, Upper : Control_Bounds; Remaining : Natural) return Boolean
      with Subprogram_Variant => (Decreases => Remaining);

      function Decide (Lower, Upper : Control_Bounds; Remaining : Natural) return Boolean is
         pragma Suppress (Overflow_Check);
         All_Nonnegative : Boolean := True;
         All_Negative    : Boolean := True;
      begin
         for Index in Control_Bounds'Range loop
            All_Nonnegative := All_Nonnegative and then Lower (Index) >= 0.0;
            All_Negative := All_Negative and then Upper (Index) < 0.0;
         end loop;
         if All_Nonnegative then
            return True;
         elsif Upper (0) < 0.0 or else Upper (Degree) < 0.0 then
            --  The end controls are the values of the polynomial at the ends of this piece, so it is negative
            --  there and no further bisection can certify it.
            return False;
         elsif All_Negative or else Remaining = 0 or else Degree = 0 then
            return False;
         end if;

         declare
            Work_Lower  : Control_Bounds := Lower;
            Work_Upper  : Control_Bounds := Upper;
            Left_Lower  : Control_Bounds := [others => 0.0];
            Left_Upper  : Control_Bounds := [others => 0.0];
            Right_Lower : Control_Bounds := [others => 0.0];
            Right_Upper : Control_Bounds := [others => 0.0];
         begin
            Left_Lower (0) := Work_Lower (0);
            Left_Upper (0) := Work_Upper (0);
            Right_Lower (Degree) := Work_Lower (Degree);
            Right_Upper (Degree) := Work_Upper (Degree);
            for Level in 1 .. Degree loop
               for Index in 0 .. Degree - Level loop
                  Work_Lower (Index) :=
                    Inflate_Finite (0.5 * (Work_Lower (Index) + Work_Lower (Index + 1)), Upward => False);
                  Work_Upper (Index) :=
                    Inflate_Finite (0.5 * (Work_Upper (Index) + Work_Upper (Index + 1)), Upward => True);
               end loop;
               Left_Lower (Level) := Work_Lower (0);
               Left_Upper (Level) := Work_Upper (0);
               Right_Lower (Degree - Level) := Work_Lower (Degree - Level);
               Right_Upper (Degree - Level) := Work_Upper (Degree - Level);
            end loop;
            return
              Decide (Left_Lower, Left_Upper, Remaining - 1) and then Decide (Right_Lower, Right_Upper, Remaining - 1);
         end;
      end Decide;

      Lower, Upper : Control_Bounds := [others => 0.0];
   begin
      for Index in Control_Bounds'Range loop
         declare
            Coefficient : Interval renames Coefficients (Coefficients'First + Index);
         begin
            --  The comparisons fail for a NaN as well.
            if not Coefficient.Valid
              or else not (abs Coefficient.Lower <= Magnitude_Limit)
              or else not (abs Coefficient.Upper <= Magnitude_Limit)
              or else not (Coefficient.Lower <= Coefficient.Upper)
            then
               return False;
            end if;
            Lower (Index) := Coefficient.Lower;
            Upper (Index) := Coefficient.Upper;
         end;
      end loop;
      return Decide (Lower, Upper, Depth);
   end Strict_Bernstein_Nonnegative;

   Corrected_Power_Limit : constant := 2.0**606;
   --  Largest power coefficient which Corrected_Power_Chart returns.

   Position_Error_Limit : constant := 2.0**100;
   --  Largest position error, in millimetres, which Build_Candidate adds to another. A larger one is treated as no
   --  bound at all, which every request refuses.

   function Corrected_Power_Chart (Power : Power_Chart; Effective_C0 : Raw_Vector_3) return Power_Chart
   with
     Pre  => Within (Power, Power_Basis_Limit) and then Within (Effective_C0, Tame_Limit),
     Post => Within (Corrected_Power_Chart'Result, Corrected_Power_Limit);
   --  Repair the endpoint-invisible closure mode as the complete polynomial V⁴·(1 - V)⁴. Altering only the V⁸
   --  coefficient would change the finish tangent jet seen by the rational compiler.

   function Corrected_Power_Chart (Power : Power_Chart; Effective_C0 : Raw_Vector_3) return Power_Chart is
      Envelope_Power : constant array (Natural range 4 .. 8) of Dimensionless := [1.0, -4.0, 6.0, -4.0, 1.0];
      Result         : Power_Chart := Power;
   begin
      for Component in Chart_Component_Index loop
         pragma Loop_Invariant (Within (Result, Corrected_Power_Limit));
         pragma Assert (abs Effective_C0 (Component) <= Tame_Limit);
         pragma Assert (abs Power (Fixed_Chart_Degree, Component) <= Power_Basis_Limit);
         declare
            Correction : constant Dimensionless :=
              256.0 * Effective_C0 (Component) - Power (Fixed_Chart_Degree, Component);
         begin
            for Degree in Envelope_Power'Range loop
               pragma Loop_Invariant (Within (Result, Corrected_Power_Limit));
               pragma Assert (abs Power (Degree, Component) <= Power_Basis_Limit);
               pragma Assert (abs Envelope_Power (Degree) <= 6.0);
               declare
                  Original : constant Dimensionless := Power (Degree, Component);
                  Weight   : constant Dimensionless := Envelope_Power (Degree);
                  Value    : constant Dimensionless := Original + Correction * Weight;
               begin
                  Set_Power_Coefficient (Result, Degree, Component, Value, Corrected_Power_Limit);
               end;
            end loop;
         end;
      end loop;
      return Result;
   end Corrected_Power_Chart;

   function Uncompiled_Evaluator (Request : Blend_Request; Length_Value : Length) return Stereographic_Curve_Evaluator
   with
     Pre  =>
       Points_Are_Moderate (Request)
       and then Length_Value >= Arc_Length_Floor
       and then Length_Value <= Arc_Length_Limit,
     Post => Uncompiled_Evaluator'Result.Kind = Positive_Curve_Kind;
   --  The evaluator of a positive curve before Build_Rational_Representation fills its cache.

   function Uncompiled_Evaluator (Request : Blend_Request; Length_Value : Length) return Stereographic_Curve_Evaluator
   is
   begin
      pragma
        Assert
          (for all Axis in Axis_Name =>
             abs Request.Start.Point (Axis) <= Coordinate_Limit
             and then abs Request.Finish.Point (Axis) <= Coordinate_Limit);
      return
        (Kind                     => Positive_Curve_Kind,
         Start_Point              => Request.Start.Point,
         Finish_Point             => Request.Finish.Point,
         Length_Value             => Length_Value,
         Antiderivative_Cache     => (others => <>),
         Uncorrected_Finish_Point => [others => 0.0 * mm]);
   end Uncompiled_Evaluator;

   function Sum_Of_Errors (Left : Length; Right : Dimensionless) return Length
   with Pre => Left >= 0.0 * mm and then Right >= 0.0, Post => Sum_Of_Errors'Result >= 0.0 * mm;
   --  An upper bound on the sum of two position error bounds, the second in millimetres. Length'Last, which is no
   --  bound, if either is beyond the limit of the construction.

   function Sum_Of_Errors (Left : Length; Right : Dimensionless) return Length is
   begin
      if not (Left <= Position_Error_Limit * mm) or else not (Right <= Position_Error_Limit) then
         return Length'Last;
      end if;
      declare
         Sum : constant Dimensionless := Dimensionless (Left / mm) + Right;
      begin
         pragma Assert (Sum >= 0.0 and then Sum <= 2.0 * Position_Error_Limit);
         return Up (Sum) * mm;
      end;
   end Sum_Of_Errors;

   function Is_Sound_Positive_Curve (Curve : Stereographic_Curve) return Boolean
   is (Curve.Evaluator_Data.Kind = Positive_Curve_Kind
       and then Curve.Certified_Position_Error >= 0.0 * mm
       and then Curve.Certified_Frame_Speed_Upper >= 0.0
       and then Is_Finite (Curve.Certified_Frame_Speed_Upper)
       and then Curve.Certified_Tangent_Error >= 0.0
       and then Is_Finite (Curve.Certified_Tangent_Error));
   --  What the predicate of a curve requires of a positive one.

   procedure Lemma_Bernstein_Chart_Within_Larger (Value : Bernstein_Chart; Smaller, Larger : Dimensionless)
   with Ghost, Pre => Within (Value, Smaller) and then Smaller <= Larger, Post => Within (Value, Larger);

   procedure Lemma_Bernstein_Chart_Within_Larger (Value : Bernstein_Chart; Smaller, Larger : Dimensionless) is null;

   procedure Lemma_Power_Chart_Is_Finite (Value : Power_Chart)
   with
     Ghost,
     Pre  => Within (Value, Corrected_Power_Limit),
     Post =>
       (for all Degree in Chart_Coefficient_Index =>
          (for all Component in Chart_Component_Index => Is_Finite (Value (Degree, Component))));

   procedure Lemma_Power_Chart_Is_Finite (Value : Power_Chart) is null;

   function Build_Candidate
     (Request                             : Blend_Request;
      Chord_Length                        : Length;
      Chord_Direction                     : Position_Scale;
      Frame                               : Frame_Vector_Array;
      Start_Chart_Jets, Finish_Chart_Jets : Chart_Jet_Array;
      Warp_Factor                         : Dimensionless;
      Initial_Closure                     : Closure_Seed) return Candidate_Result
   is
      Ideal : constant Ideal_Candidate :=
        Build_Ideal_Candidate
          (Request,
           Chord_Length,
           Chord_Direction,
           Frame,
           Start_Chart_Jets,
           Finish_Chart_Jets,
           Warp_Factor,
           Initial_Closure);
   begin
      if Ideal.Status = Candidate_Numerically_Unsafe then
         return (Status => Candidate_Numerically_Unsafe);
      elsif Ideal.Status /= Candidate_Success then
         return (Status => Candidate_Closure_Failed);
      elsif not Points_Are_Moderate (Request) then
         --  The evaluator holds no coordinate beyond the limit of the construction.
         return (Status => Candidate_Numerically_Unsafe);
      end if;
      Lemma_Bernstein_Chart_Within_Larger (Ideal.Coefficients, Tame_Limit, Chart_Build_Limit);

      declare
         Coefficients                : Bernstein_Chart renames Ideal.Coefficients;
         Rational_Power_Coefficients : constant Power_Chart :=
           Corrected_Power_Chart (Power_Basis (Coefficients), Ideal.Effective_C0);
         Frame_Speed_Upper           : constant Dimensionless := Frame_Speed_Upper_Bound (Frame);
         Compilation                 : Realtime_Compilation_Status;
         Curve                       : Stereographic_Curve :=
           (Evaluator_Data               => Uncompiled_Evaluator (Request, Ideal.Length_Value),
            Frame                        => Frame,
            Coefficients                 => Coefficients,
            Warp_Factor                  => Distance_Warp_Factor (Warp_Factor),
            Certified_Frame_Speed_Upper  => Frame_Speed_Upper,
            Certified_Position_Error     => 0.0 * mm,
            Certified_Tangent_Error      => 0.0,
            Requested_Start_Jet          => Request.Start.Jet,
            Requested_Finish_Jet         => Request.Finish.Jet,
            Retained_Tangent_Certificate => (others => <>),
            Geometry                     => <>,
            Evaluator_Evidence           => <>,
            Whole_Curve_Majorants        => [others => [others => 0.0]],
            Has_Whole_Curve_Majorants    => False,
            Structurally_Constant_Axes   => [others => False],
            Bounds                       => (others => <>));
         Correction_Error_Raw        : Dimensionless;
      begin
         if Frame_Speed_Upper <= 0.0
           or else Frame_Speed_Upper >= Dimensionless'Last
           or else not Is_Finite (Frame_Speed_Upper)
         then
            return (Status => Candidate_Numerically_Unsafe);
         end if;

         --  Allowing bulge is a relaxation of the default geometry, not a switch to a disjoint acceptance rule.
         --  Retain a certified no-bulge candidate first; only a candidate which leaves the endpoint tangent
         --  cone needs the chord-monotonicity proof. This matters for obtuse asymmetric line corners, whose
         --  fixed endpoint tangent can point backwards along the chord even though the complete curve stays
         --  inside its tangent cone.
         Curve.Geometry :=
           Stereographic_Kernel.Certified_Geometry.Certify (Frame, Coefficients, Ideal.Bulge_Allowed, Chord_Direction);
         if Stereographic_Kernel.Certified_Geometry.Kind (Curve.Geometry)
           = Stereographic_Kernel.Certified_Geometry.Not_Certified
         then
            return (Status => Candidate_Closure_Failed);
         end if;
         --  Only after ideal closure and geometry are certified is the compact rational evaluator compiled and
         --  compared against the authoritative chart.
         Lemma_Power_Chart_Is_Finite (Rational_Power_Coefficients);
         Compilation :=
           Build_Rational_Representation
             (Curve,
              Rational_Power_Coefficients,
              Request.Maximum_Position_Error,
              Request.Start.Jet,
              Request.Finish.Jet);
         case Compilation is
            when Realtime_Compilation_Succeeded          =>
               null;

            when Realtime_Representation_Insufficient    =>
               return (Status => Candidate_Representation_Failed);

            when Realtime_Compilation_Numerically_Unsafe =>
               return (Status => Candidate_Numerically_Unsafe);
         end case;
         if not Is_Sound_Positive_Curve (Curve) then
            --  Never: compilation fills the cache of the positive curve it was given and leaves its certificates
            --  as the type requires. GNATprove assumes nothing about an object which a function with side
            --  effects has updated, so this says it.
            return (Status => Candidate_Numerically_Unsafe);
         end if;

         declare
            Public_Roundoff            : constant Length := Realtime_Point_Roundoff_Bound (Curve.Evaluator_Data);
            Error_With_Public_Roundoff : constant Length :=
              Sum_Of_Errors (Curve.Certified_Position_Error, Dimensionless (Public_Roundoff / mm));
         begin
            --  The stored error budget is cumulative:
            --
            --     representation error + public evaluation roundoff + continuous endpoint correction.
            --
            --  Charge each term before exposing the curve.
            if Public_Roundoff = Length'Last
              or else not Is_Finite (Dimensionless (Public_Roundoff / mm))
              or else not Is_Finite (Dimensionless (Error_With_Public_Roundoff / mm))
            then
               return (Status => Candidate_Numerically_Unsafe);
            elsif Error_With_Public_Roundoff > Request.Maximum_Position_Error then
               return (Status => Candidate_Representation_Failed);
            end if;
            Curve.Certified_Position_Error := Error_With_Public_Roundoff;
         end;

         Correction_Error_Raw := Finish_Correction_Position_Error_Bound (Curve.Evaluator_Data);

         declare
            Total_Error : constant Length := Sum_Of_Errors (Curve.Certified_Position_Error, Correction_Error_Raw);
         begin
            if Correction_Error_Raw = Dimensionless'Last
              or else not Is_Finite (Correction_Error_Raw)
              or else not Is_Finite (Dimensionless (Total_Error / mm))
            then
               return (Status => Candidate_Numerically_Unsafe);
            elsif Total_Error > Request.Maximum_Position_Error then
               --  Representation and public evaluator roundoff already passed their independent budget checks
               --  above. Any remaining excess is the continuous endpoint correction, so this is a closure failure
               --  rather than a representation failure.
               return (Status => Candidate_Closure_Failed);
            end if;
            Curve.Certified_Position_Error := Total_Error;
         end;
         declare
            Sharp    : Boolean;
            Whole    : Axis_Majorants;
            Retained : Boolean;
         begin
            Whole := Pole_Taylor_Tangent_Range_Majorants (Curve, 0.0, 1.0, Sharp);
            --  As Realtime_Tangent_Range_Majorants, except that a certificate reconstructed for the Bernstein
            --  fallback is kept, so later range queries on this curve need not reconstruct it again.
            if not Sharp then
               Retained := Curve.Retained_Tangent_Certificate.Valid;
               if not Retained then
                  Retained := Retain_Bernstein_Tangent_Certificate (Curve);
                  if not Is_Sound_Positive_Curve (Curve) then
                     --  Never, as above: retaining a certificate changes nothing else.
                     return (Status => Candidate_Numerically_Unsafe);
                  end if;
               end if;
               --  A retained certificate is a valid one, so the second test never fails.
               if Retained and then Curve.Retained_Tangent_Certificate.Valid then
                  Merge_Bernstein_Tangent_Range_Majorants (Curve, 0.0, 1.0, Whole);
               end if;
            end if;
            Curve.Whole_Curve_Majorants := Whole;
         end;
         for Axis in Axis_Name loop
            for Order in Majorant_Order loop
               if Curve.Whole_Curve_Majorants (Axis) (Order) >= Dimensionless'Last
                 or else not Is_Finite (Curve.Whole_Curve_Majorants (Axis) (Order))
               then
                  return (Status => Candidate_Numerically_Unsafe);
               end if;
            end loop;
         end loop;
         Curve.Has_Whole_Curve_Majorants := True;
         Curve.Bounds := Bounds_On_Parameter_Range (Curve, 0.0, 1.0);
         for Axis in Axis_Name loop
            if Curve.Bounds.Velocity (Axis) >= Dimensionless'Last
              or else Curve.Bounds.Acceleration (Axis) >= Curvature'Last
              or else Curve.Bounds.Jerk (Axis) >= Curvature_To_2'Last
              or else Curve.Bounds.Snap (Axis) >= Curvature_To_3'Last
              or else Curve.Bounds.Crackle (Axis) >= Curvature_To_4'Last
            then
               return (Status => Candidate_Numerically_Unsafe);
            end if;
         end loop;
         return (Status => Candidate_Success, Curve => Curve);
      end;
   end Build_Candidate;

   function Axis_Is_Structurally_Constant (Curve : Stereographic_Curve; Axis : Axis_Name) return Boolean is
   begin
      return Curve.Evaluator_Data.Kind = Zero_Curve_Kind or else Curve.Structurally_Constant_Axes (Axis);
   end Axis_Is_Structurally_Constant;

   Miss_Term_Limit : constant := 256.0;
   --  Above what one quadrature node adds to either integral of Flat_Line_Blend_Miss_Ratio.

   function Next_Miss_Sum (Sum, Term : Dimensionless; Terms_Before : Natural) return Dimensionless
   is (Sum + Term)
   with
     Pre  =>
       Terms_Before < 8
       and then abs Sum <= Dimensionless (Terms_Before) * Miss_Term_Limit
       and then abs Term <= Miss_Term_Limit,
     Post => abs Next_Miss_Sum'Result <= Dimensionless (Terms_Before + 1) * Miss_Term_Limit;

   procedure Miss_Node_Terms
     (Node, Weight, Chart : Dimensionless; Upper_Half : Boolean; Rise_Term, Advance_Term : out Dimensionless)
   with
     Pre  =>
       Node >= 0.0
       and then Node <= 1.0
       and then Weight >= 0.0
       and then Weight <= 1.0
       and then Chart >= 0.0
       and then Chart <= 1.0,
     Post => abs Rise_Term <= Miss_Term_Limit and then abs Advance_Term <= Miss_Term_Limit;
   --  The weighted sine and cosine of the tangent's angle with the chord at one quadrature node.

   procedure Miss_Node_Terms
     (Node, Weight, Chart : Dimensionless; Upper_Half : Boolean; Rise_Term, Advance_Term : out Dimensionless)
   is
      Offset : constant Dimensionless := 0.25 * Node;
      pragma Assert (Offset >= 0.0 and then Offset <= 0.25);
      U      : constant Dimensionless := (if Upper_Half then 0.25 + Offset else 0.25 - Offset);
      pragma Assert (U >= 0.0 and then U <= 0.5);
      Square : constant Dimensionless := U * U;
      Inner  : constant Dimensionless := 70.0 - 20.0 * U;
      Middle : constant Dimensionless := -84.0 + U * Inner;
      Outer  : constant Dimensionless := 35.0 + U * Middle;
      pragma Assert (abs Inner <= 70.0 and then abs Middle <= 119.0 and then abs Outer <= 95.0);
      Step   : constant Dimensionless := (Square * Square) * Outer;
      pragma Assert (abs Step <= 8.0);
      Y      : constant Dimensionless := Chart * (1.0 - 2.0 * Step);
   begin
      pragma Assert (abs Y <= 17.0);
      Rise_Term := Weight * (2.0 * Y / (1.0 + Y * Y));
      Advance_Term := Weight * ((1.0 - Y * Y) / (1.0 + Y * Y));
   end Miss_Node_Terms;

   function Flat_Line_Blend_Miss_Ratio (Cos_Half_Theta : Dimensionless) return Dimensionless is
      --  With flat endpoint jets and equal trims the chart is t·(1 - 2·σ(U)), where t = tan (θ/4) and σ is the
      --  degree-seven smoothstep, and no closure mode is needed. Its tangent makes the angle φ with the chord, where
      --  cos φ = (1 - y²)/(1 + y²) and sin φ = 2·y/(1 + y²), so integrating both over half the curve locates its
      --  middle, which is its nearest point to the corner:
      --
      --     miss/trim = sin (θ/2) - cos (θ/2) · ∫ sin φ dU / ∫ cos φ dU   over [0, 1/2].
      --
      --  Both integrands are smooth and an eight-point Gauss-Legendre rule resolves the ratio to seven digits. Its
      --  weights would also scale both integrals by 1/4, which cancels.
      Nodes     : constant array (1 .. 4) of Dimensionless :=
        [0.183_434_642_495_649_8, 0.525_532_409_916_329_0, 0.796_666_477_413_626_7, 0.960_289_856_497_536_3];
      Weights   : constant array (1 .. 4) of Dimensionless :=
        [0.362_683_783_378_362_0, 0.313_706_645_877_887_3, 0.222_381_034_453_374_5, 0.101_228_536_290_376_3];
      Cosine    : constant Dimensionless := Dimensionless'Max (0.0, Dimensionless'Min (1.0, Cos_Half_Theta));
      Remainder : constant Dimensionless := 1.0 - Cosine * Cosine;
      Sine      : Dimensionless;
      Chart     : Dimensionless;
      Rise      : Dimensionless := 0.0;
      Advance   : Dimensionless := 0.0;
      Count     : Natural := 0;
   begin
      pragma Assert (Remainder >= 0.0 and then Remainder <= 1.0);
      Stereographic_Kernel.Elementary.Lemma_Sqrt_Is_At_Most (Remainder, 1.0);
      Sine := Dimensionless_Math.Sqrt (Remainder);
      Chart := Dimensionless'Min (1.0, Sine / (1.0 + Cosine));
      for Index in Nodes'Range loop
         pragma Loop_Invariant (Count = 2 * (Index - 1));
         pragma
           Loop_Invariant
             (abs Rise <= Dimensionless (Count) * Miss_Term_Limit
              and then abs Advance <= Dimensionless (Count) * Miss_Term_Limit);
         pragma Assert (Nodes (Index) >= 0.0 and then Nodes (Index) <= 1.0);
         pragma Assert (Weights (Index) >= 0.0 and then Weights (Index) <= 1.0);
         for Upper_Half in Boolean loop
            pragma Loop_Invariant (Count = 2 * (Index - 1) + Boolean'Pos (Upper_Half));
            pragma
              Loop_Invariant
                (abs Rise <= Dimensionless (Count) * Miss_Term_Limit
                 and then abs Advance <= Dimensionless (Count) * Miss_Term_Limit);
            declare
               Rise_Term, Advance_Term : Dimensionless;
            begin
               Miss_Node_Terms (Nodes (Index), Weights (Index), Chart, Upper_Half, Rise_Term, Advance_Term);
               Rise := Next_Miss_Sum (Rise, Rise_Term, Count);
               Advance := Next_Miss_Sum (Advance, Advance_Term, Count);
               Count := Count + 1;
            end;
         end loop;
      end loop;
      pragma Assert (abs Rise <= 8.0 * Miss_Term_Limit);
      --  The integral of the cosine is about a quarter or more for every angle, so the comparison only excludes
      --  a quotient which could not be formed.
      if Advance > 2.0**(-20) then
         return Dimensionless'Max (0.0, Sine - Cosine * (Rise / Advance));
      else
         return 1.0;
      end if;
   end Flat_Line_Blend_Miss_Ratio;

   Warp_Half_Floor : constant := 2.0**(-20);
   --  Least cosine and sine of half the angle between the endpoint tangents for which a warp is solved. The
   --  conditioning test of Select_Distance_Warp leaves both about five times larger.

   Warp_Denominator_Floor : constant := 2.0**(-25);
   --  Below the least value of the warp's denominator W·(1 - V) + V, which lies between one and W.

   Warp_Ratio_Limit : constant := 64.0;
   --  Above the largest trim ratio, and its reciprocal below the smallest, which pass the asymmetry test.

   Warp_Node_Count : constant := 64;
   --  The nodes of the warp quadrature: four panels of sixteen.

   Warp_Term_Limit : constant := 2.0**170;
   Warp_Sum_Limit  : constant := 2.0**177;
   --  Largest term of a warp quadrature sum, and largest sum of an endpoint value and the terms of all nodes.

   Warp_Slope_Floor : constant := 2.0**(-200);
   --  Smallest magnitude of the residual's derivative which a Newton step of the warp solve divides by.

   Warp_Integral_Floor : constant := 2.0**(-200);
   --  Smallest integral which the trim coordinate is divided by to propose a length ratio.

   function Is_Warp_Shape (Q, Cos_Half, Sin_Half : Dimensionless) return Boolean
   is (Q >= 0.0
       and then Q <= 1.0
       and then Cos_Half >= Warp_Half_Floor
       and then Cos_Half <= 1.0
       and then Sin_Half >= Warp_Half_Floor
       and then Sin_Half <= 1.0);
   --  Whether these are the tangent of a quarter, and the cosine and sine of half, of a well conditioned angle
   --  between the endpoint tangents.

   function Endpoint_Jets_Are_Flat (Request : Blend_Request) return Boolean;
   --  Test whether all requested endpoint tangent derivatives through order three vanish.

   function Endpoint_Jets_Are_Flat (Request : Blend_Request) return Boolean is
   begin
      for Axis in Axis_Name loop
         if Request.Start.Jet.Tangent_Derivative_1 (Axis) /= 0.0 / mm
           or else Request.Start.Jet.Tangent_Derivative_2 (Axis) /= 0.0 / mm**2
           or else Request.Start.Jet.Tangent_Derivative_3 (Axis) /= 0.0 / mm**3
           or else Request.Finish.Jet.Tangent_Derivative_1 (Axis) /= 0.0 / mm
           or else Request.Finish.Jet.Tangent_Derivative_2 (Axis) /= 0.0 / mm**2
           or else Request.Finish.Jet.Tangent_Derivative_3 (Axis) /= 0.0 / mm**3
         then
            return False;
         end if;
      end loop;
      return True;
   end Endpoint_Jets_Are_Flat;

   procedure Warp_Node_Terms
     (Candidate_Warp, Q, Cos_Half, Sin_Half        : Dimensionless;
      Reference_Start, Reference_Finish, V, Weight : Dimensionless;
      A_Term, B_Term, D_A_Term, D_B_Term           : out Dimensionless)
   with
     Pre  =>
       Candidate_Warp >= Minimum_Distance_Warp_Factor
       and then Candidate_Warp <= Maximum_Distance_Warp_Factor
       and then Is_Warp_Shape (Q, Cos_Half, Sin_Half)
       and then Reference_Start >= 0.0
       and then Reference_Start <= 1.0
       and then Reference_Finish >= 0.0
       and then Reference_Finish <= 1.0
       and then V >= 0.0
       and then V <= 1.0
       and then Weight >= 0.0
       and then Weight <= 1.0,
     Post =>
       abs A_Term <= Warp_Term_Limit
       and then abs B_Term <= Warp_Term_Limit
       and then abs D_A_Term <= Warp_Term_Limit
       and then abs D_B_Term <= Warp_Term_Limit;
   pragma Inline (Warp_Node_Terms);
   --  What one quadrature node at chart coordinate V adds to the four sums of Integrate_Warp.

   procedure Warp_Node_Terms
     (Candidate_Warp, Q, Cos_Half, Sin_Half        : Dimensionless;
      Reference_Start, Reference_Finish, V, Weight : Dimensionless;
      A_Term, B_Term, D_A_Term, D_B_Term           : out Dimensionless)
   is
      pragma Suppress (Overflow_Check);
      --  Every quantity here is bounded: W lies in its supported range, the warp denominator between one and W,
      --  and the tangent decomposition by the conditioning limit of Select_Distance_Warp.

      Raw_Denominator    : constant Dimensionless := Candidate_Warp - (Candidate_Warp - 1.0) * V;
      Warp_Denominator   : constant Dimensionless :=
        (if Raw_Denominator >= Warp_Denominator_Floor then Raw_Denominator else Warp_Denominator_Floor);
      --  The floor is below every value of W·(1 - V) + V, so it never replaces one. It states the size of the
      --  divisor for the proof, which the two terms of the difference do not show.
      Warp_Jacobian      : Dimensionless;
      D_Warp_Jacobian    : Dimensionless;
      --  D_Warp_Jacobian is ∂(dU/dV)/∂log(W), which supplies the derivative needed by the safeguarded Newton
      --  solve.
      V2                 : constant Dimensionless := V * V;
      H                  : Dimensionless;
      --  H is the degree-7 smoothstep
      --
      --     H(V) = V⁴·(35 - 84·V + 70·V² - 20·V³).
      --
      --  Its first three derivatives vanish at V = 0 and V = 1. Z moves from -Q to +Q; inverse stereographic
      --  projection then traces the unit tangent from s to f.
      Z, Z2              : Dimensionless;
      Stereo_Denominator : Dimensionless;
      X, Y               : Dimensionless;
      --  Resolve the planar unit tangent into the endpoint basis:
      --
      --     T(V) = Start_Coefficient·s + Finish_Coefficient·f.
      Start_Coefficient  : Dimensionless;
      Finish_Coefficient : Dimensionless;
      Start_Delta        : Dimensionless;
      Finish_Delta       : Dimensionless;
   begin
      pragma Assert (Raw_Denominator <= 2.0**25);
      pragma Assert (Warp_Denominator >= Warp_Denominator_Floor and then Warp_Denominator <= 2.0**25);
      pragma Assert (Warp_Denominator * Warp_Denominator >= 2.0**(-50));
      Warp_Jacobian := Candidate_Warp / (Warp_Denominator * Warp_Denominator);
      pragma Assert (Warp_Jacobian >= 0.0 and then Warp_Jacobian <= 2.0**74);
      pragma Assert (abs (2.0 * Candidate_Warp * (1.0 - V) / Warp_Denominator) <= 2.0**50);
      D_Warp_Jacobian := Warp_Jacobian * (1.0 - 2.0 * Candidate_Warp * (1.0 - V) / Warp_Denominator);
      pragma Assert (abs D_Warp_Jacobian <= 2.0**125);
      pragma Assert (V2 >= 0.0 and then V2 <= 1.0);
      H := (V2 * V2) * (35.0 + V * (-84.0 + V * (70.0 - 20.0 * V)));
      pragma Assert (abs H <= 64.0);
      Z := Q * (2.0 * H - 1.0);
      pragma Assert (abs Z <= 129.0);
      Z2 := Z * Z;
      pragma Assert (Z2 >= 0.0 and then Z2 <= 2.0**15);
      Stereo_Denominator := 1.0 + Z2;
      pragma Assert (Stereo_Denominator >= 1.0 and then Stereo_Denominator <= 2.0**16);
      X := (1.0 - Z2) / Stereo_Denominator;
      Y := 2.0 * Z / Stereo_Denominator;
      pragma Assert (abs X <= 2.0**15 and then abs Y <= 2.0**9);
      pragma Assert (abs (X / Cos_Half) <= 2.0**35 and then abs (Y / Sin_Half) <= 2.0**29);
      Start_Coefficient := 0.5 * (X / Cos_Half - Y / Sin_Half);
      Finish_Coefficient := 0.5 * (X / Cos_Half + Y / Sin_Half);
      pragma Assert (abs Start_Coefficient <= 2.0**35 and then abs Finish_Coefficient <= 2.0**35);
      Start_Delta := Start_Coefficient - Reference_Start;
      Finish_Delta := Finish_Coefficient - Reference_Finish;
      pragma Assert (abs Start_Delta <= 2.0**36 and then abs Finish_Delta <= 2.0**36);
      pragma Assert (Weight * Warp_Jacobian >= 0.0 and then Weight * Warp_Jacobian <= 2.0**74);
      pragma Assert (abs (Weight * D_Warp_Jacobian) <= 2.0**125);
      A_Term := Weight * Warp_Jacobian * Start_Delta;
      B_Term := Weight * Warp_Jacobian * Finish_Delta;
      D_A_Term := Weight * D_Warp_Jacobian * Start_Delta;
      D_B_Term := Weight * D_Warp_Jacobian * Finish_Delta;
   end Warp_Node_Terms;

   function Next_Warp_Sum (Sum, Term : Dimensionless; Nodes_Before : Natural) return Dimensionless
   with
     Pre  =>
       Nodes_Before < Warp_Node_Count
       and then abs Sum <= Dimensionless (Nodes_Before + 1) * Warp_Term_Limit
       and then abs Term <= Warp_Term_Limit,
     Post => abs Next_Warp_Sum'Result <= Dimensionless (Nodes_Before + 2) * Warp_Term_Limit;
   pragma Inline (Next_Warp_Sum);
   --  A warp quadrature sum with the term of one more node.

   function Next_Warp_Sum (Sum, Term : Dimensionless; Nodes_Before : Natural) return Dimensionless is
      pragma Suppress (Overflow_Check);
      pragma Unreferenced (Nodes_Before);
   begin
      return Sum + Term;
   end Next_Warp_Sum;

   procedure Add_Warp_Node
     (Candidate_Warp, Q, Cos_Half, Sin_Half        : Dimensionless;
      Reference_Start, Reference_Finish, V, Weight : Dimensionless;
      A, B, D_A, D_B                               : in out Dimensionless;
      Nodes_Before                                 : Natural)
   with
     Pre  =>
       Candidate_Warp >= Minimum_Distance_Warp_Factor
       and then Candidate_Warp <= Maximum_Distance_Warp_Factor
       and then Is_Warp_Shape (Q, Cos_Half, Sin_Half)
       and then Reference_Start >= 0.0
       and then Reference_Start <= 1.0
       and then Reference_Finish >= 0.0
       and then Reference_Finish <= 1.0
       and then V >= 0.0
       and then V <= 1.0
       and then Weight >= 0.0
       and then Weight <= 1.0
       and then Nodes_Before < Warp_Node_Count
       and then abs A <= Dimensionless (Nodes_Before + 1) * Warp_Term_Limit
       and then abs B <= Dimensionless (Nodes_Before + 1) * Warp_Term_Limit
       and then abs D_A <= Dimensionless (Nodes_Before + 1) * Warp_Term_Limit
       and then abs D_B <= Dimensionless (Nodes_Before + 1) * Warp_Term_Limit,
     Post =>
       abs A <= Dimensionless (Nodes_Before + 2) * Warp_Term_Limit
       and then abs B <= Dimensionless (Nodes_Before + 2) * Warp_Term_Limit
       and then abs D_A <= Dimensionless (Nodes_Before + 2) * Warp_Term_Limit
       and then abs D_B <= Dimensionless (Nodes_Before + 2) * Warp_Term_Limit;
   pragma Inline (Add_Warp_Node);
   --  Add one quadrature node to the four sums of Integrate_Warp.

   procedure Add_Warp_Node
     (Candidate_Warp, Q, Cos_Half, Sin_Half        : Dimensionless;
      Reference_Start, Reference_Finish, V, Weight : Dimensionless;
      A, B, D_A, D_B                               : in out Dimensionless;
      Nodes_Before                                 : Natural)
   is
      A_Term, B_Term, D_A_Term, D_B_Term : Dimensionless;
   begin
      Warp_Node_Terms
        (Candidate_Warp,
         Q,
         Cos_Half,
         Sin_Half,
         Reference_Start,
         Reference_Finish,
         V,
         Weight,
         A_Term,
         B_Term,
         D_A_Term,
         D_B_Term);
      A := Next_Warp_Sum (A, A_Term, Nodes_Before);
      B := Next_Warp_Sum (B, B_Term, Nodes_Before);
      D_A := Next_Warp_Sum (D_A, D_A_Term, Nodes_Before);
      D_B := Next_Warp_Sum (D_B, D_B_Term, Nodes_Before);
   end Add_Warp_Node;

   procedure Integrate_Warp
     (Candidate_Log_Warp, Q, Cos_Half, Sin_Half : Dimensionless; A, B, D_A, D_B : out Dimensionless)
   with
     Pre  =>
       abs Candidate_Log_Warp <= Stereographic_Kernel.Elementary.Warp_Log_Max
       and then Is_Warp_Shape (Q, Cos_Half, Sin_Half),
     Post =>
       abs A <= Warp_Sum_Limit
       and then abs B <= Warp_Sum_Limit
       and then abs D_A <= Warp_Sum_Limit
       and then abs D_B <= Warp_Sum_Limit;
   --  For W = exp(Candidate_Log_Warp), integrate the ideal tangent transition as
   --
   --     ∫₀¹ T(U) dU = A(W)·s + B(W)·f,
   --
   --  and also return dA/dlog(W) and dB/dlog(W). Working in log(W) guarantees W > 0 and treats reciprocal warps
   --  symmetrically.

   procedure Integrate_Warp
     (Candidate_Log_Warp, Q, Cos_Half, Sin_Half : Dimensionless; A, B, D_A, D_B : out Dimensionless)
   is
      Candidate_Warp   : constant Dimensionless := Stereographic_Kernel.Elementary.Warp_Exp (Candidate_Log_Warp);
      Panel_Count      : constant Positive := 4;
      --  Integrate in the unwarped chart coordinate V, using
      --
      --     U(V)   = V / (W·(1 - V) + V),
      --     dU/dV  = W / (W - (W - 1)·V)².
      --
      --  For a strong warp, dU/dV is concentrated at one endpoint. Subtract that endpoint tangent before applying
      --  the Jacobian and add its exactly known integral back. The degree-seven flat chart makes the remaining
      --  integrand vanish to fourth order in the concentrated boundary layer, so fixed GL16 work remains accurate
      --  even at the 20:1 trim limit.
      Reference_Start  : constant Dimensionless := (if Candidate_Warp < 1.0 then 1.0 else 0.0);
      Reference_Finish : constant Dimensionless := (if Candidate_Warp < 1.0 then 0.0 else 1.0);
   begin
      A := Reference_Start;
      B := Reference_Finish;
      D_A := 0.0;
      D_B := 0.0;
      for Panel in 0 .. Panel_Count - 1 loop
         pragma
           Loop_Invariant
             (abs A <= Dimensionless (16 * Panel + 1) * Warp_Term_Limit
              and then abs B <= Dimensionless (16 * Panel + 1) * Warp_Term_Limit
              and then abs D_A <= Dimensionless (16 * Panel + 1) * Warp_Term_Limit
              and then abs D_B <= Dimensionless (16 * Panel + 1) * Warp_Term_Limit);
         declare
            Middle : constant Dimensionless := (Dimensionless (Panel) + 0.5) / Dimensionless (Panel_Count);
            Half   : constant Dimensionless := 0.5 / Dimensionless (Panel_Count);
         begin
            pragma Assert (Middle >= 0.125 and then Middle <= 0.875 and then Half = 0.125);
            for I in GL16_Positive_Index loop
               pragma
                 Loop_Invariant
                   (abs A <= Dimensionless (16 * Panel + 2 * (I - 1) + 1) * Warp_Term_Limit
                    and then abs B <= Dimensionless (16 * Panel + 2 * (I - 1) + 1) * Warp_Term_Limit
                    and then abs D_A <= Dimensionless (16 * Panel + 2 * (I - 1) + 1) * Warp_Term_Limit
                    and then abs D_B <= Dimensionless (16 * Panel + 2 * (I - 1) + 1) * Warp_Term_Limit);
               pragma Assert (GL16_Nodes (I) >= 0.0 and then GL16_Nodes (I) <= 1.0);
               pragma Assert (GL16_Weights (I) >= 0.0 and then GL16_Weights (I) <= 1.0);
               declare
                  Left_V  : constant Dimensionless := Middle - Half * GL16_Nodes (I);
                  Right_V : constant Dimensionless := Middle + Half * GL16_Nodes (I);
                  Weight  : constant Dimensionless := Half * GL16_Weights (I);
               begin
                  pragma Assert (Left_V >= 0.0 and then Left_V <= 1.0);
                  pragma Assert (Right_V >= 0.0 and then Right_V <= 1.0);
                  pragma Assert (Weight >= 0.0 and then Weight <= 1.0);
                  Add_Warp_Node
                    (Candidate_Warp,
                     Q,
                     Cos_Half,
                     Sin_Half,
                     Reference_Start,
                     Reference_Finish,
                     Left_V,
                     Weight,
                     A,
                     B,
                     D_A,
                     D_B,
                     16 * Panel + 2 * (I - 1));
                  Add_Warp_Node
                    (Candidate_Warp,
                     Q,
                     Cos_Half,
                     Sin_Half,
                     Reference_Start,
                     Reference_Finish,
                     Right_V,
                     Weight,
                     A,
                     B,
                     D_A,
                     D_B,
                     16 * Panel + 2 * (I - 1) + 1);
               end;
            end loop;
         end;
      end loop;
      pragma
        Assert
          (abs A <= 65.0 * Warp_Term_Limit
           and then abs B <= 65.0 * Warp_Term_Limit
           and then abs D_A <= 65.0 * Warp_Term_Limit
           and then abs D_B <= 65.0 * Warp_Term_Limit);
   end Integrate_Warp;

   procedure Warp_Shape (G : Dimensionless; Q, Cos_Half, Sin_Half : out Dimensionless; Conditioned : out Boolean)
   with Pre => abs G < 1.0, Post => (if Conditioned then Is_Warp_Shape (Q, Cos_Half, Sin_Half));
   --  For unit endpoint tangents whose scalar product is G = cos(θ), Cos_Half = cos(θ/2), Sin_Half = sin(θ/2) and
   --  Q = tan(θ/4), the stereographic coordinate of either endpoint relative to the angle bisector. Conditioned is
   --  False where the cosine or the sine is below Warp_Half_Floor.

   procedure Warp_Shape (G : Dimensionless; Q, Cos_Half, Sin_Half : out Dimensionless; Conditioned : out Boolean) is
      Cos_Square : constant Dimensionless := Dimensionless'Max (0.0, 0.5 * (1.0 + G));
      Sin_Square : constant Dimensionless := Dimensionless'Max (0.0, 0.5 * (1.0 - G));
   begin
      Q := 0.0;
      Cos_Half := 1.0;
      Sin_Half := 1.0;
      Conditioned := False;
      if not (Cos_Square >= Warp_Half_Floor * Warp_Half_Floor)
        or else not (Sin_Square >= Warp_Half_Floor * Warp_Half_Floor)
      then
         return;
      end if;
      pragma Assert (Cos_Square <= 1.0 and then Sin_Square <= 1.0);
      Stereographic_Kernel.Elementary.Lemma_Sqrt_Is_At_Most (Cos_Square, 1.0);
      Stereographic_Kernel.Elementary.Lemma_Sqrt_Is_At_Least (Cos_Square, Warp_Half_Floor);
      Stereographic_Kernel.Elementary.Lemma_Sqrt_Is_At_Most (Sin_Square, 1.0);
      Stereographic_Kernel.Elementary.Lemma_Sqrt_Is_At_Least (Sin_Square, Warp_Half_Floor);
      Cos_Half := Dimensionless_Math.Sqrt (Cos_Square);
      Sin_Half := Dimensionless_Math.Sqrt (Sin_Square);
      Q := Sin_Half / (1.0 + Cos_Half);
      Conditioned := True;
   end Warp_Shape;

   procedure Trim_Coordinates
     (Chord_Direction, Start_Tangent, Finish_Tangent : Position_Scale;
      G, Denominator                                 : Dimensionless;
      Incoming, Outgoing                             : out Dimensionless)
   with
     Pre  =>
       Within (Chord_Direction, Unit_Component_Limit)
       and then Within (Start_Tangent, 4.0)
       and then Within (Finish_Tangent, 4.0)
       and then abs G <= 1.0
       and then Denominator >= 2.0**(-100),
     Post => abs Incoming <= Vector_Dot_Limit and then abs Outgoing <= Vector_Dot_Limit;
   --  Construct the dual vectors
   --
   --     s* = (s - G·f) / (1 - G²),
   --     f* = (f - G·s) / (1 - G²).
   --
   --  They obey s*·s = f*·f = 1 and s*·f = f*·s = 0. Consequently Incoming = c·s* and Outgoing = c·f* are the
   --  chord coordinates in the oblique endpoint-tangent basis:
   --
   --     c ≈ Incoming·s + Outgoing·f.

   procedure Trim_Coordinates
     (Chord_Direction, Start_Tangent, Finish_Tangent : Position_Scale;
      G, Denominator                                 : Dimensionless;
      Incoming, Outgoing                             : out Dimensionless) is
   begin
      Lemma_Within_Larger (Chord_Direction, Unit_Component_Limit, Vector_Quotient_Limit);
      Lemma_Within_Larger (Start_Tangent, 4.0, Tame_Limit);
      Lemma_Within_Larger (Finish_Tangent, 4.0, Tame_Limit);
      Lemma_Within_Larger (Start_Tangent, 4.0, Vector_Wide_Limit);
      Lemma_Within_Larger (Finish_Tangent, 4.0, Vector_Wide_Limit);
      declare
         Start_Difference  : constant Position_Scale := Minus_Scaled (Start_Tangent, Finish_Tangent, G);
         Finish_Difference : constant Position_Scale := Minus_Scaled (Finish_Tangent, Start_Tangent, G);
      begin
         Lemma_Within_Larger (Start_Difference, Vector_Wide_Limit, 2.0 * Vector_Wide_Limit);
         Lemma_Within_Larger (Finish_Difference, Vector_Wide_Limit, 2.0 * Vector_Wide_Limit);
         declare
            Start_Dual  : constant Position_Scale := Divided (Start_Difference, Denominator);
            Finish_Dual : constant Position_Scale := Divided (Finish_Difference, Denominator);
         begin
            Incoming := Vector_Dot (Chord_Direction, Start_Dual);
            Outgoing := Vector_Dot (Chord_Direction, Finish_Dual);
         end;
      end;
   end Trim_Coordinates;

   procedure Solve_Warp_Logarithm
     (Ratio, Q, Cos_Half, Sin_Half     : Dimensionless;
      Log_Warp, Integral_A, Integral_B : out Dimensionless;
      Status                           : out Warp_Selection_Status)
   with
     Pre  =>
       Ratio >= 1.0 / Warp_Ratio_Limit
       and then Ratio <= Warp_Ratio_Limit
       and then Is_Warp_Shape (Q, Cos_Half, Sin_Half),
     Post =>
       abs Log_Warp <= Stereographic_Kernel.Elementary.Warp_Log_Max
       and then abs Integral_A <= Warp_Sum_Limit
       and then abs Integral_B <= Warp_Sum_Limit
       and then (if Status = Warp_Was_Selected then Integral_A > 0.0 and then Integral_B > 0.0);
   --  With zero closure bubble, the degree-seven endpoint-flat chart is monotone in the angle-bisector frame.
   --  Closure for some length ratio λ requires
   --
   --     λ·A(W) = Incoming,     λ·B(W) = Outgoing.
   --
   --  Eliminating λ leaves the scalar equation
   --
   --     F(log(W)) = B(W) - R·A(W) = 0
   --
   --  for the trim ratio R. A and B vary monotonically with log(W), so the root is unique. Safeguarded Newton
   --  retains fixed bounded construction cost, and the wide log bracket covers the complete advertised 20:1 trim
   --  domain near reversals. On success the integrals are those of the selected W.

   procedure Solve_Warp_Logarithm
     (Ratio, Q, Cos_Half, Sin_Half     : Dimensionless;
      Log_Warp, Integral_A, Integral_B : out Dimensionless;
      Status                           : out Warp_Selection_Status)
   is
      Log_Low  : Dimensionless := Distance_Warp_Log_Lower_Bound;
      Log_High : Dimensionless := Distance_Warp_Log_Upper_Bound;

      Warp_Is_Evaluated : Boolean := False;
      --  Whether Integral_A and Integral_B were integrated at the final Log_Warp.
   begin
      Stereographic_Kernel.Elementary.Lemma_Log_Is_Bounded (Ratio);
      Log_Warp := Dimensionless'Max (Log_Low, Dimensionless'Min (Log_High, 1.2 * Dimensionless_Math.Log (Ratio)));
      Integral_A := 0.0;
      Integral_B := 0.0;

      --  The residual increases with log(W), so the bracket's ends only matter to a solve which reaches them.
      --  Run the Newton iteration first with that direction taken for granted. While every proposal stays
      --  strictly inside the bracket it contracts, the iterates are exactly those of the safeguarded solve
      --  below, which is left to settle every other case, with the ends evaluated, from the start.
      declare
         Trial      : Dimensionless := Log_Warp;
         Trial_Low  : Dimensionless := Log_Low;
         Trial_High : Dimensionless := Log_High;
      begin
         for Iteration in 1 .. 56 loop
            pragma
              Loop_Invariant
                (Trial_Low >= Distance_Warp_Log_Lower_Bound
                 and then Trial_High <= Distance_Warp_Log_Upper_Bound
                 and then abs Log_Warp <= Stereographic_Kernel.Elementary.Warp_Log_Max
                 and then abs Integral_A <= Warp_Sum_Limit
                 and then abs Integral_B <= Warp_Sum_Limit
                 and then not Warp_Is_Evaluated);
            declare
               D_A, D_B             : Dimensionless;
               Residual, D_Residual : Dimensionless;
               Proposed             : Dimensionless;
               Residual_Scale       : Dimensionless;
            begin
               exit when not (Trial > Trial_Low and then Trial < Trial_High);
               Integrate_Warp (Trial, Q, Cos_Half, Sin_Half, Integral_A, Integral_B, D_A, D_B);
               Residual := Integral_B - Ratio * Integral_A;
               D_Residual := D_B - Ratio * D_A;
               Residual_Scale := Dimensionless'Max (1.0, abs Integral_B + abs (Ratio * Integral_A));
               exit when not Is_Finite (Residual);
               if abs Residual <= 2_048.0 * Dimensionless'Model_Epsilon * Residual_Scale then
                  Log_Warp := Trial;
                  Warp_Is_Evaluated := True;
                  exit;
               end if;
               exit when Trial_High - Trial_Low <= 2.0E-13;
               if Residual < 0.0 then
                  Trial_Low := Trial;
               else
                  Trial_High := Trial;
               end if;
               --  A slope below the floor would propose a step far outside the bracket, which ends this
               --  iteration as well.
               exit when
                 D_Residual = 0.0 or else not Is_Finite (D_Residual) or else not (abs D_Residual >= Warp_Slope_Floor);
               Proposed := Trial - Residual / D_Residual;
               exit when not Is_Finite (Proposed);
               Trial := Proposed;
            end;
         end loop;
      end;

      if not Warp_Is_Evaluated then
         declare
            Low_A, Low_B, Low_D_A, Low_D_B     : Dimensionless;
            High_A, High_B, High_D_A, High_D_B : Dimensionless;
            Low_Residual, High_Residual        : Dimensionless;
            Residual_Increases                 : Boolean := False;
         begin
            Integrate_Warp (Log_Low, Q, Cos_Half, Sin_Half, Low_A, Low_B, Low_D_A, Low_D_B);
            Integrate_Warp (Log_High, Q, Cos_Half, Sin_Half, High_A, High_B, High_D_A, High_D_B);
            Low_Residual := Low_B - Ratio * Low_A;
            High_Residual := High_B - Ratio * High_A;
            --  The endpoint residuals must be finite and must bracket zero. Equal signs mean no supported W
            --  reproduces R.
            if not Is_Finite (Low_Residual) or else not Is_Finite (High_Residual) then
               Status := Warp_Selection_Is_Numerically_Unsafe;
               return;
            elsif Low_Residual = 0.0 then
               Log_Warp := Log_Low;
            elsif High_Residual = 0.0 then
               Log_Warp := Log_High;
            elsif (Low_Residual < 0.0) = (High_Residual < 0.0) then
               Status := Warp_Closure_Failed;
               return;
            else
               Residual_Increases := Low_Residual < High_Residual;
            end if;

            if Low_Residual /= 0.0 and then High_Residual /= 0.0 then
               --  Each iteration first contracts the proven bracket, then takes
               --
               --     log(W) ← log(W) - F/F′.
               --
               --  A nonfinite or out-of-bracket Newton proposal is replaced by the bisection midpoint, and so is
               --  one which a slope below the floor would send far outside the bracket.
               for Iteration in 1 .. 56 loop
                  pragma
                    Loop_Invariant
                      (Log_Low >= Distance_Warp_Log_Lower_Bound
                       and then Log_Low <= Distance_Warp_Log_Upper_Bound
                       and then Log_High >= Distance_Warp_Log_Lower_Bound
                       and then Log_High <= Distance_Warp_Log_Upper_Bound
                       and then abs Log_Warp <= Stereographic_Kernel.Elementary.Warp_Log_Max
                       and then abs Integral_A <= Warp_Sum_Limit
                       and then abs Integral_B <= Warp_Sum_Limit
                       and then not Warp_Is_Evaluated);
                  declare
                     D_A, D_B             : Dimensionless;
                     Residual, D_Residual : Dimensionless;
                     Proposed, Midpoint   : Dimensionless;
                     Residual_Scale       : Dimensionless;
                  begin
                     Integrate_Warp (Log_Warp, Q, Cos_Half, Sin_Half, Integral_A, Integral_B, D_A, D_B);
                     Residual := Integral_B - Ratio * Integral_A;
                     D_Residual := D_B - Ratio * D_A;
                     Residual_Scale := Dimensionless'Max (1.0, abs Integral_B + abs (Ratio * Integral_A));
                     if abs Residual <= 2_048.0 * Dimensionless'Model_Epsilon * Residual_Scale
                       or else Log_High - Log_Low <= 2.0E-13
                     then
                        --  The integrals above are those of the selected W.
                        Warp_Is_Evaluated := True;
                        exit;
                     elsif (Residual < 0.0) = Residual_Increases then
                        Log_Low := Log_Warp;
                     else
                        Log_High := Log_Warp;
                     end if;

                     Midpoint := 0.5 * (Log_Low + Log_High);
                     pragma Assert (abs Midpoint <= Stereographic_Kernel.Elementary.Warp_Log_Max);
                     Proposed :=
                       (if D_Residual /= 0.0
                          and then Is_Finite (D_Residual)
                          and then abs D_Residual >= Warp_Slope_Floor
                        then Log_Warp - Residual / D_Residual
                        else Midpoint);
                     if not Is_Finite (Proposed) or else Proposed <= Log_Low or else Proposed >= Log_High then
                        Proposed := Midpoint;
                     end if;
                     pragma Assert (abs Proposed <= Stereographic_Kernel.Elementary.Warp_Log_Max);
                     Log_Warp := Proposed;
                  end;
               end loop;
            end if;
         end;
      end if;

      declare
         D_A, D_B       : Dimensionless;
         Final_Residual : Dimensionless;
         Final_Scale    : Dimensionless;
      begin
         --  Evaluate the selected W, unless the solve stopped on it and its integrals are already those of this
         --  W. A valid result needs positive finite A and B and must reproduce B/A = R within the final scaled
         --  roundoff allowance.
         pragma Assert (abs Log_Warp <= Stereographic_Kernel.Elementary.Warp_Log_Max);
         if not Warp_Is_Evaluated then
            Integrate_Warp (Log_Warp, Q, Cos_Half, Sin_Half, Integral_A, Integral_B, D_A, D_B);
         end if;
         pragma Assert (abs Integral_A <= Warp_Sum_Limit and then abs Integral_B <= Warp_Sum_Limit);
         Final_Residual := Integral_B - Ratio * Integral_A;
         Final_Scale := Dimensionless'Max (1.0, abs Integral_B + abs (Ratio * Integral_A));
         if not Is_Finite (Integral_A)
           or else not Is_Finite (Integral_B)
           or else not Is_Finite (Final_Residual)
           or else not Is_Finite (Final_Scale)
           or else Integral_A <= 0.0
           or else Integral_B <= 0.0
         then
            Status := Warp_Selection_Is_Numerically_Unsafe;
         elsif abs Final_Residual > 8_192.0 * Dimensionless'Model_Epsilon * Final_Scale then
            Status := Warp_Closure_Failed;
         else
            Status := Warp_Was_Selected;
         end if;
      end;
   end Solve_Warp_Logarithm;

   function Flat_Closure_Seed (Incoming, Integral_A, Maximum_Lambda : Dimensionless) return Closure_Seed
   with
     Pre =>
       abs Incoming <= Vector_Dot_Limit
       and then abs Integral_A <= Warp_Sum_Limit
       and then Maximum_Lambda >= 1.0
       and then Maximum_Lambda <= Lambda_Limit;
   --  For structurally planar flat endpoint jets, the zero-bubble construction as a proposal when it respects the
   --  arc-length cap: λ = Incoming / A(W). Build_Candidate canonicalizes the admitted normal noise and includes its
   --  continuous endpoint correction in the executed derivative bounds.

   function Flat_Closure_Seed (Incoming, Integral_A, Maximum_Lambda : Dimensionless) return Closure_Seed is
      Seed : Closure_Seed;
   begin
      --  An integral below the floor would give a length ratio far beyond any cap.
      if Integral_A > 0.0 and then Integral_A >= Warp_Integral_Floor then
         declare
            Lambda : constant Dimensionless := Incoming / Integral_A;
         begin
            if Is_Finite (Lambda) and then Lambda >= 1.0 and then Lambda <= Maximum_Lambda then
               Seed := (Valid => True, Lambda => Lambda, C0 => [others => 0.0]);
            end if;
         end;
      end if;
      return Seed;
   end Flat_Closure_Seed;

   function Select_Distance_Warp
     (Request                                        : Blend_Request;
      Chord_Direction, Start_Tangent, Finish_Tangent : Position_Scale;
      Maximum_Lambda                                 : Dimensionless) return Warp_Selection
   is
      --  U is normalized physical distance and V is the coordinate of the polynomial tangent chart. Select W in the
      --  endpoint-preserving map
      --
      --     V(U) = W·U / (1 - U + W·U).
      --
      --  W = 1 is the identity, W < 1 spends more distance near the start tangent, and W > 1 spends more distance
      --  near the finish tangent.

      --  For unit endpoint tangents s and f, G = s·f = cos(θ) and 1 - G² is the determinant of their Gram matrix.
      G                  : Dimensionless;
      Denominator        : Dimensionless;
      Incoming, Outgoing : Dimensionless;
      Scale              : Dimensionless;
      Ratio              : Dimensionless;
      --  The required outgoing-to-incoming trim ratio.
      Q                  : Dimensionless;
      Cos_Half           : Dimensionless;
      Sin_Half           : Dimensionless;
      Conditioned        : Boolean;
   begin
      Lemma_Within_Larger (Start_Tangent, 4.0, Vector_Quotient_Limit);
      Lemma_Within_Larger (Finish_Tangent, 4.0, Vector_Quotient_Limit);
      G := Vector_Dot (Start_Tangent, Finish_Tangent);
      --  Nearly parallel or antiparallel endpoint tangents make the dual basis below ill-conditioned. In that case
      --  there is no trustworthy scalar trim decomposition, so retain the identity map. A scalar product of one
      --  or more in magnitude leaves no positive determinant either.
      if not (abs G < 1.0) then
         return (Status => Warp_Was_Selected, Factor => 1.0, Seed => <>);
      end if;
      Denominator := 1.0 - G * G;
      if Denominator <= 1.0E-10 then
         return (Status => Warp_Was_Selected, Factor => 1.0, Seed => <>);
      end if;
      Trim_Coordinates (Chord_Direction, Start_Tangent, Finish_Tangent, G, Denominator, Incoming, Outgoing);
      Scale := Dimensionless'Max (1.0, Dimensionless'Max (abs Incoming, abs Outgoing));

      if Incoming > 0.0 and then Outgoing > 0.0 then
         --  The scalar warp describes an ordinary corner only when both tangent-basis coordinates are positive.
         --  Enforce
         --
         --     1 / Maximum_Trim_Asymmetry
         --        ≤ Outgoing / Incoming
         --        ≤ Maximum_Trim_Asymmetry.
         --
         --  Check the requested cap before selecting the identity map for numerically negligible asymmetry.
         --  Otherwise a positive but very short leg could bypass the advertised 20:1 limit.
         if Incoming > Maximum_Trim_Asymmetry_Check * Outgoing
           or else Outgoing > Maximum_Trim_Asymmetry_Check * Incoming
         then
            return (Status => Warp_Trim_Asymmetry_Is_Unsupported, others => <>);
         elsif Incoming <= 1.0E-12 * Scale or else Outgoing <= 1.0E-12 * Scale then
            return (Status => Warp_Was_Selected, Factor => 1.0, Seed => <>);
         end if;
      else
         return (Status => Warp_Was_Selected, Factor => 1.0, Seed => <>);
      end if;

      Ratio := Outgoing / Incoming;
      if not (Ratio >= 1.0 / Warp_Ratio_Limit and then Ratio <= Warp_Ratio_Limit) then
         --  Never: the asymmetry test above leaves a ratio between a twentieth and twenty.
         return (Status => Warp_Trim_Asymmetry_Is_Unsupported, others => <>);
      end if;
      Warp_Shape (G, Q, Cos_Half, Sin_Half, Conditioned);
      if not Conditioned then
         --  Never: the determinant test above leaves the cosine and the sine of half the angle well above the
         --  floor. Were it otherwise there would be no trustworthy decomposition either.
         return (Status => Warp_Was_Selected, Factor => 1.0, Seed => <>);
      end if;

      declare
         --  A zero-bubble chart is a useful closure proposal only when the requested endpoint tangent jets are
         --  flat through order 3 and the physical chord is in their plane up to floating-point backward error.
         Chord_In_Endpoint_Plane : constant Boolean :=
           Chord_Is_Numerically_In_Endpoint_Plane (Request, Chord_Direction, Start_Tangent, Finish_Tangent);
         Integral_A, Integral_B  : Dimensionless;
         Log_Warp                : Dimensionless;
         Status                  : Warp_Selection_Status;
      begin
         --  Equal trim coordinates give R = 1 and, by symmetry, W = 1. Avoid all scalar-warp work on this
         --  overwhelmingly common planner path. For the endpoint-flat zero-bubble proposal, symmetry gives
         --
         --     λ = Incoming / A(1) = Outgoing / B(1).
         if abs (Outgoing - Incoming) <= 128.0 * Dimensionless'Model_Epsilon * Dimensionless'Max (Incoming, Outgoing)
         then
            if Endpoint_Jets_Are_Flat (Request) and then Chord_In_Endpoint_Plane then
               declare
                  D_A, D_B : Dimensionless;
               begin
                  Integrate_Warp (0.0, Q, Cos_Half, Sin_Half, Integral_A, Integral_B, D_A, D_B);
                  return
                    (Status => Warp_Was_Selected,
                     Factor => 1.0,
                     Seed   => Flat_Closure_Seed (Incoming, Integral_A, Maximum_Lambda));
               end;
            else
               return (Status => Warp_Was_Selected, Factor => 1.0, Seed => <>);
            end if;
         end if;

         Solve_Warp_Logarithm (Ratio, Q, Cos_Half, Sin_Half, Log_Warp, Integral_A, Integral_B, Status);
         case Status is
            when Warp_Was_Selected                    =>
               null;

            when Warp_Trim_Asymmetry_Is_Unsupported   =>
               return (Status => Warp_Trim_Asymmetry_Is_Unsupported, others => <>);

            when Warp_Closure_Failed                  =>
               return (Status => Warp_Closure_Failed, others => <>);

            when Warp_Selection_Is_Numerically_Unsafe =>
               return (Status => Warp_Selection_Is_Numerically_Unsafe, others => <>);
         end case;
         declare
            Seed : Closure_Seed;
         begin
            if Endpoint_Jets_Are_Flat (Request) and then Chord_In_Endpoint_Plane then
               Seed := Flat_Closure_Seed (Incoming, Integral_A, Maximum_Lambda);
            end if;
            return
              (Status => Warp_Was_Selected,
               Factor => Stereographic_Kernel.Elementary.Warp_Exp (Log_Warp),
               Seed   => Seed);
         end;
      end;
   end Select_Distance_Warp;

   function Chord_Offset (Request : Blend_Request) return Position_Offset
   with
     Pre  => Points_Are_Moderate (Request),
     Post => (for all Axis in Axis_Name => abs Chord_Offset'Result (Axis) <= Tame_Limit * mm);
   --  The finish point less the start point.

   function Chord_Offset (Request : Blend_Request) return Position_Offset is
      Result : Position_Offset := [others => 0.0 * mm];
   begin
      for Axis in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => abs Result (B) <= Tame_Limit * mm);
         pragma
           Assert
             (abs Request.Start.Point (Axis) <= Coordinate_Limit
              and then abs Request.Finish.Point (Axis) <= Coordinate_Limit);
         declare
            Component : constant Length := Request.Finish.Point (Axis) - Request.Start.Point (Axis);
         begin
            pragma Assert (abs Component <= Tame_Limit * mm);
            Result (Axis) := Component;
         end;
      end loop;
      return Result;
   end Chord_Offset;

   function Chord_Direction_Of (Chord : Position_Offset; Chord_Length : Length) return Position_Scale
   with
     Pre  => Chord_Length > 0.0 * mm and then (for all Axis in Axis_Name => abs Chord (Axis) <= Chord_Length),
     Post => Within (Chord_Direction_Of'Result, 1.0);
   --  The chord divided by its length, which is at least each of its components.

   function Chord_Direction_Of (Chord : Position_Offset; Chord_Length : Length) return Position_Scale is
      Result : Position_Scale := [others => 0.0];
   begin
      for Axis in Axis_Name loop
         pragma Loop_Invariant (for all B in Axis_Name => abs Result (B) <= 1.0);
         pragma Assert (abs Chord (Axis) <= Chord_Length);
         declare
            Component : constant Dimensionless := Chord (Axis) / Chord_Length;
         begin
            pragma Assert (abs Component <= 1.0);
            Result (Axis) := Component;
         end;
      end loop;
      return Result;
   end Chord_Direction_Of;

   function Added (Left, Right : Position_Scale) return Position_Scale
   with
     Pre  => Within (Left, 12.0) and then Within (Right, 4.0),
     Post => Within (Added'Result, 16.0) and then (if Within (Left, 4.0) then Within (Added'Result, 8.0));
   --  The sum of two vectors of a few unit vectors' size.

   function Added (Left, Right : Position_Scale) return Position_Scale is
      Result : Position_Scale := [others => 0.0];
   begin
      for Axis in Axis_Name loop
         pragma
           Loop_Invariant
             (for all B in Axis_Name =>
                abs Result (B) <= 16.0 and then (if Within (Left, 4.0) then abs Result (B) <= 8.0));
         pragma Assert (abs Left (Axis) <= 12.0 and then abs Right (Axis) <= 4.0);
         pragma Assert (if Within (Left, 4.0) then abs Left (Axis) <= 4.0);
         declare
            Component : constant Dimensionless := Left (Axis) + Right (Axis);
         begin
            pragma Assert (abs Component <= 16.0);
            pragma Assert (if abs Left (Axis) <= 4.0 then abs Component <= 8.0);
            Result (Axis) := Component;
         end;
      end loop;
      return Result;
   end Added;

   function Directions_Are_Unit (Directions : Frame_Candidate_Array) return Boolean
   is (for all I in Directions'Range => Within (Directions (I), Unit_Component_Limit));

   procedure Set_Direction (Directions : in out Frame_Candidate_Array; Index : Positive; Value : Position_Scale)
   with
     Pre  => Index in Directions'Range and then Directions_Are_Unit (Directions) and then Within (Value, 1.0),
     Post => Directions_Are_Unit (Directions);

   procedure Set_Direction (Directions : in out Frame_Candidate_Array; Index : Positive; Value : Position_Scale) is
   begin
      Lemma_Within_Larger (Value, 1.0, Unit_Component_Limit);
      Directions (Index) := Value;
   end Set_Direction;

   procedure Add_Direction
     (Directions       : in out Frame_Candidate_Array;
      Direction_Count  : in out Natural;
      Seeded_Direction : in out Natural;
      Candidate        : Position_Scale;
      Seeded           : Boolean)
   with
     Pre  =>
       Directions'First = 1
       and then Directions'Last = Blend_Frame_Index'Last
       and then Direction_Count < Blend_Frame_Index'Last
       and then Seeded_Direction <= Blend_Frame_Index'Last
       and then Directions_Are_Unit (Directions)
       and then Within (Candidate, Candidate_Component_Limit),
     Post =>
       Directions_Are_Unit (Directions)
       and then Direction_Count >= Direction_Count'Old
       and then Direction_Count <= Direction_Count'Old + 1
       and then Seeded_Direction <= Blend_Frame_Index'Last;
   --  Add the unit vector along Candidate to the frame directions unless it is too short to normalize or nearly
   --  the same as one already there. Nearly identical first frame vectors produce the same basis after
   --  Gram-Schmidt, so this avoids repeating an equivalent closure and compilation attempt.

   procedure Add_Direction
     (Directions       : in out Frame_Candidate_Array;
      Direction_Count  : in out Natural;
      Seeded_Direction : in out Natural;
      Candidate        : Position_Scale;
      Seeded           : Boolean)
   is
      Candidate_Norm : Dimensionless;
      Norm_OK        : Boolean;
      Duplicate      : Boolean := False;
   begin
      Candidate_Norm := Safe_Norm (Candidate, Norm_OK);
      if Norm_OK and then Candidate_Norm > 256.0 * Dimensionless'Model_Epsilon then
         declare
            Direction : constant Position_Scale := Normalized (Candidate, Candidate_Norm);
         begin
            Lemma_Within_Larger (Direction, 1.0, Vector_Quotient_Limit);
            for I in 1 .. Direction_Count loop
               pragma Assert (Within (Directions (I), Unit_Component_Limit));
               Lemma_Within_Larger (Directions (I), Unit_Component_Limit, Vector_Quotient_Limit);
               if 1.0 - Vector_Dot (Direction, Directions (I)) <= 512.0 * Dimensionless'Model_Epsilon then
                  Duplicate := True;
               end if;
            end loop;

            if not Duplicate then
               Direction_Count := Direction_Count + 1;
               Set_Direction (Directions, Direction_Count, Direction);
               if Seeded then
                  Seeded_Direction := Direction_Count;
               end if;
            end if;
         end;
      end if;
   end Add_Direction;

   function Failed_Preparation (Kind : Blend_Result_Kind) return Blend_Preparation
   is ((Status => Kind, others => <>))
   with Pre => Kind /= Blend_Success, Post => Failed_Preparation'Result.Status = Kind;

   function Prepared
     (Chord_Length                      : Length;
      Chord_Direction                   : Position_Scale;
      Start_Canonical, Finish_Canonical : Scaled_Tangent_Jet;
      Selection                         : Warp_Selection;
      Directions                        : Frame_Candidate_Array;
      Direction_Count, Seeded_Direction : Natural) return Blend_Preparation
   is ((Status           => Blend_Success,
        Chord_Length     => Chord_Length,
        Chord_Direction  => Chord_Direction,
        Start_Canonical  => Start_Canonical,
        Finish_Canonical => Finish_Canonical,
        Selection        => Selection,
        Directions       => Directions,
        Direction_Count  => Direction_Count,
        Seeded_Direction => Seeded_Direction))
   with
     Pre  =>
       Chord_Length >= Arc_Length_Floor
       and then Chord_Length <= Chord_Length_Limit
       and then Selection.Status = Warp_Was_Selected
       and then Within (Chord_Direction, Unit_Component_Limit)
       and then Within (Start_Canonical, Tame_Limit)
       and then Within (Start_Canonical (0), 4.0)
       and then Within (Finish_Canonical, Tame_Limit)
       and then Within (Finish_Canonical (0), 4.0)
       and then Directions'First = 1
       and then Directions'Last = Blend_Frame_Index'Last
       and then Directions_Are_Unit (Directions)
       and then Direction_Count <= Blend_Frame_Index'Last
       and then Seeded_Direction <= Blend_Frame_Index'Last,
     Post =>
       Prepared'Result.Status = Blend_Success
       and then Prepared'Result.Chord_Length = Chord_Length
       and then Is_Prepared (Prepared'Result);
   --  The successful preparation with the given parts.

   function Prepare_Blend (Request : Blend_Request) return Blend_Preparation is
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Is_Prepared);
      --  Prepared establishes Is_Prepared for the result. The proof of the postcondition needs that fact as it
      --  stands, not unfolded over every way of returning.

      Chord_Length                      : Length;
      Chord_Direction                   : Position_Scale;
      Start_Canonical, Finish_Canonical : Scaled_Tangent_Jet;
      Selection                         : Warp_Selection;
      Norm_OK                           : Boolean;
      Jet_OK                            : Boolean;
      Coordinate_Scale                  : Dimensionless := 1.0;
   begin
      --  A coordinate beyond the limit of the construction, as a NaN is, is no point of a machine.
      for A in Axis_Name loop
         if not (abs Request.Start.Point (A) <= Coordinate_Limit) then
            return Failed_Preparation (Blend_Invalid_Start_Point);
         end if;
         pragma
           Loop_Invariant (for all B in Axis_Name => (if B <= A then abs Request.Start.Point (B) <= Coordinate_Limit));
      end loop;

      for A in Axis_Name loop
         if not (abs Request.Finish.Point (A) <= Coordinate_Limit) then
            return Failed_Preparation (Blend_Invalid_Finish_Point);
         end if;
         pragma
           Loop_Invariant
             (for all B in Axis_Name => (if B <= A then abs Request.Finish.Point (B) <= Coordinate_Limit));
      end loop;
      pragma Assert (Points_Are_Moderate (Request));

      declare
         Chord : constant Position_Offset := Chord_Offset (Request);
      begin
         Chord_Length := Safe_Norm (Chord, Norm_OK);

         --  Judge endpoint separation relative to the absolute coordinate scale. This rejects a chord that
         --  disappeared in subtraction even when its computed norm is nonzero.
         for A in Axis_Name loop
            pragma Loop_Invariant (Coordinate_Scale >= 1.0 and then Coordinate_Scale <= Tame_Limit);
            pragma
              Assert
                (abs Request.Start.Point (A) <= Coordinate_Limit
                 and then abs Request.Finish.Point (A) <= Coordinate_Limit);
            Coordinate_Scale :=
              Dimensionless'Max
                (Coordinate_Scale,
                 Dimensionless'Max
                   (abs Dimensionless (Request.Start.Point (A) / mm),
                    abs Dimensionless (Request.Finish.Point (A) / mm)));
         end loop;

         if not Norm_OK
           or else Dimensionless (Chord_Length / mm) <= 256.0 * Dimensionless'Model_Epsilon * Coordinate_Scale
         then
            return Failed_Preparation (Blend_Endpoints_Too_Close);
         end if;
         pragma Assert (Chord_Length >= Arc_Length_Floor and then Chord_Length <= Chord_Length_Limit);

         Jet_OK := Canonicalize_And_Validate_Jet (Request.Start.Jet, Chord_Length, Start_Canonical);
         if not Jet_OK then
            return Failed_Preparation (Blend_Invalid_Start_Jets);
         end if;
         Jet_OK := Canonicalize_And_Validate_Jet (Request.Finish.Jet, Chord_Length, Finish_Canonical);
         if not Jet_OK then
            return Failed_Preparation (Blend_Invalid_Finish_Jets);
         elsif not (Request.Maximum_Position_Error > 0.0 * mm
                    and then Request.Maximum_Position_Error <= Arc_Length_Limit)
         then
            --  An error bound beyond the limit of the construction, as a NaN is, is none of a machine's.
            return Failed_Preparation (Blend_Invalid_Position_Error);
         elsif not (Request.Maximum_Arc_Length >= Chord_Length and then Request.Maximum_Arc_Length <= Arc_Length_Limit)
         then
            return Failed_Preparation (Blend_Invalid_Arc_Length_Limit);
         end if;

         Chord_Direction := Chord_Direction_Of (Chord, Chord_Length);
      end;
      Lemma_Within_Larger (Chord_Direction, 1.0, Unit_Component_Limit);
      Lemma_Within_Larger (Chord_Direction, 1.0, Candidate_Component_Limit);

      if Request.Bulge = Never and then not No_Bulge_Is_Possible (Request, Chord_Direction) then
         return Failed_Preparation (Blend_Closure_Failed);
      end if;

      --  The closure solve considers no curve longer than Lambda_Limit times its chord, whatever the request allows.
      Selection :=
        Select_Distance_Warp
          (Request,
           Chord_Direction,
           Start_Canonical (0),
           Finish_Canonical (0),
           Dimensionless'Min (Lambda_Limit, Dimensionless (Request.Maximum_Arc_Length / Chord_Length)));
      case Selection.Status is
         when Warp_Trim_Asymmetry_Is_Unsupported | Warp_Closure_Failed =>
            return Failed_Preparation (Blend_Closure_Failed);

         when Warp_Selection_Is_Numerically_Unsafe                     =>
            return Failed_Preparation (Blend_Numerically_Unsafe);

         when Warp_Was_Selected                                        =>
            null;
      end case;

      --  Try the angle bisector first, then chord-biased and endpoint-biased frame directions. The distance warp is
      --  geometry-wide and remains fixed while only the stereographic projection pole changes.
      declare
         Start_Tangent  : constant Position_Scale := Start_Canonical (0);
         Finish_Tangent : constant Position_Scale := Finish_Canonical (0);
         Directions     : Frame_Candidate_Array (Blend_Frame_Index) := [others => [others => 0.0]];
         Count          : Natural := 0;
         Seeded         : Natural := 0;
      begin
         pragma Assert (Within (Start_Tangent, 4.0) and then Within (Finish_Tangent, 4.0));
         Lemma_Within_Larger (Chord_Direction, 1.0, 4.0);
         Lemma_Within_Larger (Chord_Direction, 4.0, 12.0);
         Lemma_Within_Larger (Start_Tangent, 4.0, 12.0);
         Lemma_Within_Larger (Finish_Tangent, 4.0, 12.0);
         Lemma_Within_Larger (Start_Tangent, 4.0, Candidate_Component_Limit);
         Lemma_Within_Larger (Finish_Tangent, 4.0, Candidate_Component_Limit);
         pragma Assert (Directions_Are_Unit (Directions));
         Add_Direction (Directions, Count, Seeded, Added (Start_Tangent, Finish_Tangent), Seeded => True);
         Add_Direction
           (Directions,
            Count,
            Seeded,
            Added (Added (Chord_Direction, Start_Tangent), Finish_Tangent),
            Seeded => False);
         Add_Direction (Directions, Count, Seeded, Chord_Direction, Seeded => False);
         Add_Direction (Directions, Count, Seeded, Added (Start_Tangent, Chord_Direction), Seeded => False);
         Add_Direction (Directions, Count, Seeded, Added (Finish_Tangent, Chord_Direction), Seeded => False);
         Add_Direction (Directions, Count, Seeded, Start_Tangent, Seeded => False);
         Add_Direction (Directions, Count, Seeded, Finish_Tangent, Seeded => False);
         return
           Prepared
             (Chord_Length, Chord_Direction, Start_Canonical, Finish_Canonical, Selection, Directions, Count, Seeded);
      end;
   end Prepare_Blend;

   function Frame_Candidate
     (Preparation               : Blend_Preparation;
      Index                     : Blend_Frame_Index;
      Frame                     : out Frame_Vector_Array;
      Start_Chart, Finish_Chart : out Chart_Jet_Array) return Boolean
   is
      Zero   : constant Raw_Taylor := [others => 0.0];
      Usable : Boolean;
   begin
      pragma Assert (Within (Zero, Tame_Limit));
      Start_Chart := Chart_Jet_Of (Zero, Zero, Zero);
      Finish_Chart := Chart_Jet_Of (Zero, Zero, Zero);
      pragma Assert (Within (Preparation.Directions (Index), Unit_Component_Limit));
      Lemma_Within_Larger (Preparation.Directions (Index), Unit_Component_Limit, Candidate_Component_Limit);
      Lemma_Within_Larger (Preparation.Start_Canonical (0), 4.0, Candidate_Component_Limit);
      Lemma_Within_Larger (Preparation.Finish_Canonical (0), 4.0, Candidate_Component_Limit);
      Lemma_Within_Larger (Preparation.Chord_Direction, Unit_Component_Limit, Candidate_Component_Limit);
      Lemma_Within_Larger (Preparation.Start_Canonical (0), 4.0, Frame_Operand_Limit);
      Lemma_Within_Larger (Preparation.Finish_Canonical (0), 4.0, Frame_Operand_Limit);
      --  Inverse stereographic projection divides by 1 + T₀. Reject frames which place either endpoint tangent too
      --  close to the projection pole T₀ = -1.
      Usable :=
        Complete_Frame
          (Preparation.Directions (Index),
           Preparation.Start_Canonical (0),
           Preparation.Finish_Canonical (0),
           Preparation.Chord_Direction,
           Frame);
      pragma Assert (Within (Frame (0), Unit_Component_Limit));
      if not Usable
        or else
          not (1.0 + Frame_Coordinate (Frame, 0, Preparation.Start_Canonical (0))
               > Minimum_Safe_Reciprocal_Denominator)
        or else
          not (1.0 + Frame_Coordinate (Frame, 0, Preparation.Finish_Canonical (0))
               > Minimum_Safe_Reciprocal_Denominator)
      then
         return False;
      end if;
      Usable := Chart_From_Canonical_Jet (Frame, Preparation.Start_Canonical, Start_Chart);
      if not Usable then
         return False;
      end if;
      Usable := Chart_From_Canonical_Jet (Frame, Preparation.Finish_Canonical, Finish_Chart);
      return Usable;
   end Frame_Candidate;

   function Frame_Seed (Preparation : Blend_Preparation; Index : Blend_Frame_Index) return Closure_Seed
   is (if Index = Preparation.Seeded_Direction then Preparation.Selection.Seed else (others => <>));

   function Construct_Blend (Request : Blend_Request) return Blend_Result is
      Preparation                : constant Blend_Preparation := Prepare_Blend (Request);
      Saw_Representation_Failure : Boolean := False;
      Saw_Numerical_Failure      : Boolean := False;
   begin
      if Preparation.Status /= Blend_Success then
         --  The empty "do" part works around a crash of gnatprove 16.1.0, which cannot build the control-flow graph
         --  of a body containing a loop and an extended return statement without one.
         return Failure : Blend_Result (Preparation.Status) do
            null;
         end return;
      end if;

      for Index in 1 .. Preparation.Direction_Count loop
         declare
            Frame                     : Frame_Vector_Array;
            Start_Chart, Finish_Chart : Chart_Jet_Array;
            Frame_Usable              : Boolean;
         begin
            Frame_Usable := Frame_Candidate (Preparation, Index, Frame, Start_Chart, Finish_Chart);
            if Frame_Usable then
               declare
                  Built : constant Candidate_Result :=
                    Build_Candidate
                      (Request,
                       Preparation.Chord_Length,
                       Preparation.Chord_Direction,
                       Frame,
                       Start_Chart,
                       Finish_Chart,
                       Preparation.Selection.Factor,
                       Frame_Seed (Preparation, Index));
               begin
                  case Built.Status is
                     when Candidate_Success               =>
                        return (Kind => Blend_Success, Curve => Built.Curve);

                     when Candidate_Closure_Failed        =>
                        null;

                     when Candidate_Representation_Failed =>
                        Saw_Representation_Failure := True;

                     when Candidate_Numerically_Unsafe    =>
                        Saw_Numerical_Failure := True;
                  end case;
               end;
            end if;
         end;
      end loop;

      --  Report the most informative failure class seen across all supported frames.
      if Saw_Representation_Failure then
         return (Kind => Blend_Representation_Failed);
      elsif Saw_Numerical_Failure then
         return (Kind => Blend_Numerically_Unsafe);
      else
         return (Kind => Blend_Closure_Failed);
      end if;
   end Construct_Blend;

   function Evaluator_Matches (Curve : Stereographic_Curve) return Boolean is
      use type Stereographic_Kernel.Certified_Geometry.Certificate;
   begin
      --  This is a function with a body, not an expression function, so that Create_Blend's postcondition is
      --  about its result as Create_Blend tested it, rather than about equalities of floating-point records.
      return
        Stereographic_Kernel.Certified_Evaluator.Geometry (Curve.Evaluator_Evidence) = Curve.Geometry
        and then Stereographic_Kernel.Certified_Evaluator.Warp (Curve.Evaluator_Evidence) = Curve.Warp_Factor
        and then
          Stereographic_Kernel.Certified_Evaluator.Cache (Curve.Evaluator_Evidence)
          = Curve.Evaluator_Data.Antiderivative_Cache;
   end Evaluator_Matches;

   package body Certified_Construction is

      function Create_Blend (Request : Blend_Request) return Blend_Result is
         Result : constant Blend_Result := Construct_Blend (Request);
      begin
         --  Whatever the construction did, a curve leaves here only with a positive length within the request's limit,
         --  a certificate, issued by the proved certificate functions, for its own frame and chart, and one for its
         --  own evaluator cache with a finite bound on the tangent error of the executed motion. None of these
         --  comparisons can fail for a curve which Construct_Blend built as intended, so the result does not change;
         --  they are what makes the postcondition a theorem rather than an intention.
         if Result.Kind = Blend_Not_Attempted then
            return (Kind => Blend_Numerically_Unsafe);
         elsif Result.Kind = Blend_Success then
            if not (Arc_Length (Result.Curve) > 0.0 * mm)
              or else not (Arc_Length (Result.Curve) <= Request.Maximum_Arc_Length)
              or else not Certificate_Matches (Result.Curve)
              or else not Evaluator_Matches (Result.Curve)
              or else
                Stereographic_Kernel.Certified_Evaluator.Kind (Result.Curve.Evaluator_Evidence)
                /= Stereographic_Kernel.Certified_Evaluator.Sampled
              or else not (Executed_Tangent_Error_Bound (Result.Curve) < Dimensionless'Last)
            then
               return (Kind => Blend_Numerically_Unsafe);
            end if;
            case Stereographic_Kernel.Certified_Geometry.Kind (Result.Curve.Geometry) is
               when Stereographic_Kernel.Certified_Geometry.Not_Certified  =>
                  return (Kind => Blend_Numerically_Unsafe);

               when Stereographic_Kernel.Certified_Geometry.In_Cone        =>
                  null;

               when Stereographic_Kernel.Certified_Geometry.Chord_Progress =>
                  if Request.Bulge = Never then
                     return (Kind => Blend_Numerically_Unsafe);
                  end if;
            end case;
         end if;
         return Result;
      end Create_Blend;

   end Certified_Construction;

   function Derivative_Bounds (Curve : Stereographic_Curve) return Unit_Speed_Axial_Derivative_Bounds is
   begin
      return (if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then (others => <>) else Curve.Bounds);
   end Derivative_Bounds;

   function Derivative_Bounds
     (Curve : Stereographic_Curve; Start_Distance, End_Distance : Length) return Unit_Speed_Axial_Derivative_Bounds is
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         return (others => <>);
      else
         declare
            Start_Raw : constant Dimensionless := Dimensionless (Start_Distance / Curve.Evaluator_Data.Length_Value);
            End_Raw   : constant Dimensionless := Dimensionless (End_Distance / Curve.Evaluator_Data.Length_Value);
            Start_U   : constant Dimensionless :=
              (if Start_Distance = 0.0 * mm
               then 0.0
               elsif Start_Distance = Curve.Evaluator_Data.Length_Value
               then 1.0
               else Dimensionless'Max (0.0, Down (Start_Raw)));
            End_U     : constant Dimensionless :=
              (if End_Distance = 0.0 * mm
               then 0.0
               elsif End_Distance = Curve.Evaluator_Data.Length_Value
               then 1.0
               else Dimensionless'Min (1.0, Up (End_Raw)));
         begin
            --  The public range is expressed in physical distance. Preserve exact curve endpoints so full-range and
            --  point-at-end queries take their intended certificate paths; widen every interior division before
            --  asking the parameter-space majorant for a proof.
            if not (Start_U <= End_U) then
               --  Never: the quotients are ordered as the distances are, and widening keeps them so. The bounds
               --  of the whole curve hold on every part of it.
               return Bounds_On_Parameter_Range (Curve, 0.0, 1.0);
            end if;
            return Bounds_On_Parameter_Range (Curve, Start_U, End_U);
         end;
      end if;
   end Derivative_Bounds;

   function Ideal_Point_At_Parameter (Curve : Stereographic_Curve; Parameter : Curve_Parameter) return Position is
      Panel_Count       : constant Positive := 16;
      Integral          : Position_Scale := [others => 0.0];
      Target_U          : constant Dimensionless := Dimensionless (Parameter);
      Target_V          : constant Dimensionless := Warp_Parameter (Target_U, Curve.Warp_Factor);
      Reference_Tangent : Position_Scale;
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind or else Parameter = 0.0 then
         return Curve.Evaluator_Data.Start_Point;
      end if;

      --  Integrate in chart coordinate V rather than merely choosing panel boundaries there and then integrating
      --  each panel in U. A strong Möbius warp can map one such panel onto most of the distance range, leaving its
      --  U-space integrand sharply concentrated at one end.
      --
      --  Since
      --
      --     U = V/(W·(1 - V) + V),
      --
      --  the chart-space integrand is Tangent (V) * dU/dV, where
      --
      --     dU/dV = W/(W - (W - 1)·V)².
      --
      --  Subtract the tangent at the end where that Jacobian is largest. The difference tends to zero there,
      --  avoiding needless loss of precision. Its exactly integrable constant part is restored after quadrature
      --  because ∫₀ᵀᵃʳᵍᵉᵗⱽ(dU/dV)dV = Target_U.
      Reference_Tangent :=
        (if Curve.Warp_Factor <= 1.0
         then Frame_Vector (Curve.Frame, Stereographic (Chart_Value (Curve.Coefficients, 0.0)))
         else Frame_Vector (Curve.Frame, Stereographic (Chart_Value (Curve.Coefficients, 1.0))));

      for Panel in 1 .. Panel_Count loop
         declare
            Left_V   : constant Dimensionless := Target_V * Dimensionless (Panel - 1) / Dimensionless (Panel_Count);
            Right_V  : constant Dimensionless := Target_V * Dimensionless (Panel) / Dimensionless (Panel_Count);
            Middle_V : constant Dimensionless := 0.5 * (Left_V + Right_V);
            Half_V   : constant Dimensionless := 0.5 * (Right_V - Left_V);
         begin
            for I in GL16_Positive_Index loop
               declare
                  Left_Node  : constant Dimensionless := Middle_V - Half_V * GL16_Nodes (I);
                  Right_Node : constant Dimensionless := Middle_V + Half_V * GL16_Nodes (I);

                  Left_Denominator  : constant Dimensionless :=
                    Curve.Warp_Factor - (Curve.Warp_Factor - 1.0) * Left_Node;
                  Right_Denominator : constant Dimensionless :=
                    Curve.Warp_Factor - (Curve.Warp_Factor - 1.0) * Right_Node;

                  Left_Jacobian  : constant Dimensionless := Curve.Warp_Factor / Left_Denominator**2;
                  Right_Jacobian : constant Dimensionless := Curve.Warp_Factor / Right_Denominator**2;

                  Left_Tangent  : constant Position_Scale :=
                    Frame_Vector (Curve.Frame, Stereographic (Chart_Value (Curve.Coefficients, Left_Node)));
                  Right_Tangent : constant Position_Scale :=
                    Frame_Vector (Curve.Frame, Stereographic (Chart_Value (Curve.Coefficients, Right_Node)));
               begin
                  Integral :=
                    Integral
                    + ((Left_Tangent - Reference_Tangent) * Left_Jacobian
                       + (Right_Tangent - Reference_Tangent) * Right_Jacobian)
                      * Dimensionless (Half_V * GL16_Weights (I));
               end;
            end loop;
         end;
      end loop;

      Integral := Integral + Reference_Tangent * Target_U;

      --  Unit-speed displacement is Length·∫₀ᵁT(u)du.
      return
        [for Axis in Axis_Name =>
           Curve.Evaluator_Data.Start_Point (Axis) + Curve.Evaluator_Data.Length_Value * Integral (Axis)];
   end Ideal_Point_At_Parameter;

   function Point_At_Parameter (Curve : Stereographic_Curve; Parameter : Curve_Parameter) return Position is
   begin
      return Point_At_Parameter (Curve.Evaluator_Data, Parameter);
   end Point_At_Parameter;

   function Point_At_Parameter (Evaluator : Stereographic_Curve_Evaluator; Parameter : Curve_Parameter) return Position
   is
   begin
      if Evaluator.Kind = Zero_Curve_Kind then
         return Evaluator.Start_Point;
      end if;
      --  The product never exceeds the length. Taking the smaller of the two says so without an argument about
      --  rounding.
      return Point_At_Distance (Evaluator, Length'Min (Evaluator.Length_Value, Evaluator.Length_Value * Parameter));
   end Point_At_Parameter;

   function Point_At_Distance (Curve : Stereographic_Curve; Distance : Length) return Position is
   begin
      return Point_At_Distance (Curve.Evaluator_Data, Distance);
   end Point_At_Distance;

   function Point_At_Distance (Evaluator : Stereographic_Curve_Evaluator; Distance : Length) return Position is
   begin
      if Evaluator.Kind = Zero_Curve_Kind then
         return Evaluator.Start_Point;
      end if;
      --  Construction makes this one continuous formula bit-exact at both endpoints. Clamp only the normalized input
      --  domain; do not substitute a separately snapped point.
      return
        Evaluate_Rational_Point
          (Evaluator,
           (if Distance <= 0.0 * mm
            then 0.0
            elsif Distance >= Evaluator.Length_Value
            then 1.0
            else Dimensionless (Distance / Evaluator.Length_Value)));
   end Point_At_Distance;

   function To_Evaluator (Curve : Stereographic_Curve) return Stereographic_Curve_Evaluator
   is (Curve.Evaluator_Data);

   function Retained_Tangent_Error_Bound (Curve : Stereographic_Curve) return Dimensionless is
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         return 0.0;
      end if;
      --  The bound on the cache's tangent error, plus what the endpoint correction can add to the tangent.
      return
        Stereographic_Kernel.Finish_Correction.Total_Tangent_Error_Bound
          (Curve.Certified_Tangent_Error,
           Finish_In_Millimetres (Curve),
           Uncorrected_Finish_In_Millimetres (Curve),
           Length_In_Millimetres (Curve));
   end Retained_Tangent_Error_Bound;

   function Retained_Endpoint_Jet_Error_Bound
     (Curve : Stereographic_Curve; Order : Endpoint_Tangent_Derivative_Order) return Dimensionless
   is
      Errors   : Endpoint_Jet_Error_Bounds;
      Measured : Boolean;
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         return 0.0;
      end if;
      Measured :=
        Measure_Evaluator_Endpoint_Jet_Error
          (Curve.Evaluator_Data, Curve.Requested_Start_Jet, Curve.Requested_Finish_Jet, Errors);
      if Measured then
         return Errors (Order);
      else
         return Dimensionless'Last;
      end if;
   end Retained_Endpoint_Jet_Error_Bound;

   function Position_Error_Bound (Curve : Stereographic_Curve) return Length is
   begin
      return (if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then 0.0 * mm else Curve.Certified_Position_Error);
   end Position_Error_Bound;

   function Projected_Tangent_Bound
     (Curve : Stereographic_Curve; Coefficients : Projection_Coefficients) return Curvature is
   begin
      return Projected_Bound_On_Parameter_Range (Curve, 0.0, 1.0, Coefficients);
   end Projected_Tangent_Bound;

   function Projected_Tangent_Bound
     (Curve : Stereographic_Curve; Start_Distance, End_Distance : Length; Coefficients : Projection_Coefficients)
      return Curvature is
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         return 0.0 / mm;
      else
         declare
            Start_Raw : constant Dimensionless := Dimensionless (Start_Distance / Curve.Evaluator_Data.Length_Value);
            End_Raw   : constant Dimensionless := Dimensionless (End_Distance / Curve.Evaluator_Data.Length_Value);
            Start_U   : constant Dimensionless := Dimensionless'Max (0.0, Down (Start_Raw));
            End_U     : constant Dimensionless := Dimensionless'Min (1.0, Up (End_Raw));
         begin
            --  Widen both floating divisions before applying the requested-range projection certificate.
            return Projected_Bound_On_Parameter_Range (Curve, Start_U, End_U, Coefficients);
         end;
      end if;
   end Projected_Tangent_Bound;

   function Zero_Blend (Point : Position) return Stereographic_Curve is
   begin
      --  The canonical zero curve stores one point, zero length, and an empty antiderivative cache. Defaulted
      --  construction-only fields remain algebraic zeros.
      return
        (Evaluator_Data =>
           (Kind                     => Zero_Curve_Kind,
            Start_Point              => Point,
            Finish_Point             => Point,
            Length_Value             => 0.0 * mm,
            Antiderivative_Cache     => (others => <>),
            Uncorrected_Finish_Point => Point),
         others         => <>);
   end Zero_Blend;

end Prunt.Motion_Planner.Stereographic_Curves;
