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

--  Polynomials in Bernstein form over the real numbers, for specification and proof only.
--
--  A polynomial of degree n is given by its n + 1 controls. Its value is defined by de Casteljau's recurrence, the
--  repeated convex combination of neighbouring controls, which is the Bernstein sum
--
--     Σᵢ Pᵢ·C(n,i)·Tⁱ·(1 - T)ⁿ⁻ⁱ.

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Stereographic_Kernel.Bernstein
  with SPARK_Mode => On, Ghost, Always_Terminates
is
   use type Prunt.Motion_Planner.Reals.Real;

   type Controls is array (Natural range <>) of Real;
   --  The controls of one polynomial, lowest index first.

   function Value (P : Controls; T : Real) return Real
   is (if P'Length = 1
       then P (P'First)
       else (1.0 - T) * Value (P (P'First .. P'Last - 1), T) + T * Value (P (P'First + 1 .. P'Last), T))
   with Pre => P'Length >= 1 and then P'Last < Natural'Last, Subprogram_Variant => (Decreases => P'Length);
   --  The value at T of the polynomial with controls P.

   function All_Within (P : Controls; Low, High : Real) return Boolean
   is (for all I in P'Range => Low <= P (I) and then P (I) <= High);

   procedure Lemma_Value_At_Ends (P : Controls)
   with
     Subprogram_Variant => (Decreases => P'Length),
     Pre                => P'Length >= 1 and then P'Last < Natural'Last,
     Post               => Value (P, 0.0) = P (P'First) and then Value (P, 1.0) = P (P'Last);
   --  A polynomial takes its first control at zero and its last control at one.

   procedure Lemma_Convex_Hull (P : Controls; T, Low, High : Real)
   with
     Subprogram_Variant => (Decreases => P'Length),
     Pre                =>
       P'Length >= 1
       and then P'Last < Natural'Last
       and then 0.0 <= T
       and then T <= 1.0
       and then All_Within (P, Low, High),
     Post               => Low <= Value (P, T) and then Value (P, T) <= High;
   --  On the unit interval a polynomial stays within any bounds which hold all of its controls.

   ---------------------------------------------------------------------------
   --  Order and linearity in the controls
   ---------------------------------------------------------------------------

   function Same_Range (P, Q : Controls) return Boolean
   is (P'First = Q'First and then P'Last = Q'Last);

   procedure Lemma_Value_Is_Monotonic (P, Q : Controls; T : Real)
   with
     Subprogram_Variant => (Decreases => P'Length),
     Pre                =>
       P'Length >= 1
       and then P'Last < Natural'Last
       and then Same_Range (P, Q)
       and then (for all I in P'Range => P (I) <= Q (I))
       and then 0.0 <= T
       and then T <= 1.0,
     Post               => Value (P, T) <= Value (Q, T);
   --  On the unit interval, raising controls cannot lower the polynomial.

   procedure Lemma_Value_Is_Additive (P, Q, Sum : Controls; T : Real)
   with
     Subprogram_Variant => (Decreases => P'Length),
     Pre                =>
       P'Length >= 1
       and then P'Last < Natural'Last
       and then Same_Range (P, Q)
       and then Same_Range (P, Sum)
       and then (for all I in P'Range => Sum (I) = P (I) + Q (I)),
     Post               => Value (Sum, T) = Value (P, T) + Value (Q, T);
   --  Adding controls adds the polynomials.

   procedure Lemma_Value_Is_Homogeneous (Factor : Real; P, Scaled : Controls; T : Real)
   with
     Subprogram_Variant => (Decreases => P'Length),
     Pre                =>
       P'Length >= 1
       and then P'Last < Natural'Last
       and then Same_Range (P, Scaled)
       and then (for all I in P'Range => Scaled (I) = Factor * P (I)),
     Post               => Value (Scaled, T) = Factor * Value (P, T);
   --  Scaling controls scales the polynomial.

   procedure Lemma_Value_Ignores_Offset (P, Q : Controls; T : Real)
   with
     Subprogram_Variant => (Decreases => P'Length),
     Pre                =>
       P'Length >= 1
       and then P'Last < Natural'Last
       and then Q'Last < Natural'Last
       and then P'Length = Q'Length
       and then (for all I in P'Range => P (I) = Q (I - P'First + Q'First)),
     Post               => Value (P, T) = Value (Q, T);
   --  The value depends on the controls in order, not on where their index range starts.

   ---------------------------------------------------------------------------
   --  Subdivision at the midpoint
   ---------------------------------------------------------------------------

   Half : constant := 0.5;

   function Left_Half (P : Controls) return Controls
   with
     Pre  => P'Length >= 1 and then P'Last < Natural'Last,
     Post =>
       Same_Range (Left_Half'Result, P)
       and then (for all K in P'Range => Left_Half'Result (K) = Value (P (P'First .. K), Half));
   --  The controls of the polynomial restricted to the first half of the unit interval: each is the value at one
   --  half of a leading run of the controls, which is the left edge of de Casteljau's triangle.

   function Right_Half (P : Controls) return Controls
   with
     Pre  => P'Length >= 1 and then P'Last < Natural'Last,
     Post =>
       Same_Range (Right_Half'Result, P)
       and then (for all K in P'Range => Right_Half'Result (K) = Value (P (K .. P'Last), Half));
   --  Likewise for the second half, from the right edge of the triangle.

   procedure Lemma_Left_Half (P : Controls; T : Real)
   with
     Subprogram_Variant => (Decreases => P'Length),
     Pre                => P'Length >= 1 and then P'Last < Natural'Last,
     Post               => Value (Left_Half (P), T) = Value (P, Half * T);

   procedure Lemma_Right_Half (P : Controls; T : Real)
   with
     Subprogram_Variant => (Decreases => P'Length),
     Pre                => P'Length >= 1 and then P'Last < Natural'Last,
     Post               => Value (Right_Half (P), T) = Value (P, Half + Half * T);

end Prunt.Motion_Planner.Stereographic_Kernel.Bernstein;
