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

package body Prunt.Motion_Planner.Stereographic_Kernel.Bernstein
  with SPARK_Mode => On
is

   procedure Lemma_Value_At_Ends (P : Controls) is
   begin
      if P'Length > 1 then
         Lemma_Value_At_Ends (P (P'First .. P'Last - 1));
         Lemma_Value_At_Ends (P (P'First + 1 .. P'Last));
      end if;
   end Lemma_Value_At_Ends;

   procedure Lemma_Convex_Hull (P : Controls; T, Low, High : Real) is
   begin
      if P'Length > 1 then
         Lemma_Convex_Hull (P (P'First .. P'Last - 1), T, Low, High);
         Lemma_Convex_Hull (P (P'First + 1 .. P'Last), T, Low, High);
         declare
            Left  : constant Real := Value (P (P'First .. P'Last - 1), T);
            Right : constant Real := Value (P (P'First + 1 .. P'Last), T);
         begin
            pragma Assert ((1.0 - T) * Low <= (1.0 - T) * Left and then T * Low <= T * Right);
            pragma Assert ((1.0 - T) * Left <= (1.0 - T) * High and then T * Right <= T * High);
         end;
      end if;
   end Lemma_Convex_Hull;

   procedure Lemma_Scaling_Keeps_Order (Factor, Low, High : Real)
   with Pre => 0.0 <= Factor and then Low <= High, Post => Factor * Low <= Factor * High;
   --  Multiplying both sides of an inequality by a factor which is not negative keeps the inequality.

   procedure Lemma_Scaling_Keeps_Order (Factor, Low, High : Real) is
   begin
      pragma Assert (0.0 <= Factor * (High - Low));
      pragma Assert (Factor * (High - Low) = Factor * High - Factor * Low);
   end Lemma_Scaling_Keeps_Order;

   procedure Lemma_Value_Is_Monotonic (P, Q : Controls; T : Real) is
   begin
      if P'Length > 1 then
         Lemma_Value_Is_Monotonic (P (P'First .. P'Last - 1), Q (Q'First .. Q'Last - 1), T);
         Lemma_Value_Is_Monotonic (P (P'First + 1 .. P'Last), Q (Q'First + 1 .. Q'Last), T);
         declare
            Left_P  : constant Real := Value (P (P'First .. P'Last - 1), T);
            Left_Q  : constant Real := Value (Q (Q'First .. Q'Last - 1), T);
            Right_P : constant Real := Value (P (P'First + 1 .. P'Last), T);
            Right_Q : constant Real := Value (Q (Q'First + 1 .. Q'Last), T);
         begin
            --  Both terms of the recurrence are ordered, as both weights are within the unit interval.
            pragma Assert (Left_P <= Left_Q and then Right_P <= Right_Q);
            Lemma_Scaling_Keeps_Order (1.0 - T, Left_P, Left_Q);
            Lemma_Scaling_Keeps_Order (T, Right_P, Right_Q);
            pragma Assert (Value (P, T) = (1.0 - T) * Left_P + T * Right_P);
            pragma Assert (Value (Q, T) = (1.0 - T) * Left_Q + T * Right_Q);
         end;
      end if;
   end Lemma_Value_Is_Monotonic;

   procedure Lemma_Value_Is_Additive (P, Q, Sum : Controls; T : Real) is
   begin
      if P'Length > 1 then
         Lemma_Value_Is_Additive
           (P (P'First .. P'Last - 1), Q (Q'First .. Q'Last - 1), Sum (Sum'First .. Sum'Last - 1), T);
         Lemma_Value_Is_Additive
           (P (P'First + 1 .. P'Last), Q (Q'First + 1 .. Q'Last), Sum (Sum'First + 1 .. Sum'Last), T);
      end if;
   end Lemma_Value_Is_Additive;

   procedure Lemma_Value_Is_Homogeneous (Factor : Real; P, Scaled : Controls; T : Real) is
   begin
      if P'Length > 1 then
         Lemma_Value_Is_Homogeneous (Factor, P (P'First .. P'Last - 1), Scaled (Scaled'First .. Scaled'Last - 1), T);
         Lemma_Value_Is_Homogeneous (Factor, P (P'First + 1 .. P'Last), Scaled (Scaled'First + 1 .. Scaled'Last), T);
      end if;
   end Lemma_Value_Is_Homogeneous;

   procedure Lemma_Value_Ignores_Offset (P, Q : Controls; T : Real) is
   begin
      if P'Length > 1 then
         Lemma_Value_Ignores_Offset (P (P'First .. P'Last - 1), Q (Q'First .. Q'Last - 1), T);
         Lemma_Value_Ignores_Offset (P (P'First + 1 .. P'Last), Q (Q'First + 1 .. Q'Last), T);
      end if;
   end Lemma_Value_Ignores_Offset;

   function Left_Half (P : Controls) return Controls is
      Result : Controls (P'Range) := [others => 0.0];
   begin
      for K in P'Range loop
         Result (K) := Value (P (P'First .. K), Half);
         pragma Loop_Invariant (for all J in P'First .. K => Result (J) = Value (P (P'First .. J), Half));
      end loop;
      return Result;
   end Left_Half;

   function Right_Half (P : Controls) return Controls is
      Result : Controls (P'Range) := [others => 0.0];
   begin
      for K in P'Range loop
         Result (K) := Value (P (K .. P'Last), Half);
         pragma Loop_Invariant (for all J in P'First .. K => Result (J) = Value (P (J .. P'Last), Half));
      end loop;
      return Result;
   end Right_Half;

   ---------------------------------------------------------------------------
   --  Subdivision at the midpoint
   ---------------------------------------------------------------------------

   procedure Lemma_Value_Recurrence (P : Controls; T : Real)
   with
     Pre  => P'Length >= 2 and then P'Last < Natural'Last,
     Post =>
       Value (P, T) = (1.0 - T) * Value (P (P'First .. P'Last - 1), T) + T * Value (P (P'First + 1 .. P'Last), T);
   --  De Casteljau's recurrence, which defines Value, stated for one polynomial with more than one control.

   procedure Lemma_Value_Recurrence (P : Controls; T : Real) is null;

   procedure Lemma_Value_Of_Aligned_Mean (Low, High, Mean : Controls; T : Real)
   with
     Pre  =>
       Mean'Length >= 1
       and then Mean'Last < Natural'Last
       and then Same_Range (Mean, Low)
       and then Same_Range (Mean, High)
       and then (for all I in Mean'Range => Mean (I) = Half * Low (I) + Half * High (I)),
     Post => Value (Mean, T) = Half * Value (Low, T) + Half * Value (High, T);
   --  Averaging the controls of two polynomials averages the polynomials.

   procedure Lemma_Value_Of_Aligned_Mean (Low, High, Mean : Controls; T : Real) is
      Half_Low  : Controls (Mean'Range) := [others => 0.0];
      Half_High : Controls (Mean'Range) := [others => 0.0];
   begin
      for I in Mean'Range loop
         Half_Low (I) := Half * Low (I);
         Half_High (I) := Half * High (I);
         pragma
           Loop_Invariant
             (for all J in Mean'First .. I => Half_Low (J) = Half * Low (J) and then Half_High (J) = Half * High (J));
      end loop;
      Lemma_Value_Is_Homogeneous (Half, Low, Half_Low, T);
      Lemma_Value_Is_Homogeneous (Half, High, Half_High, T);
      Lemma_Value_Is_Additive (Half_Low, Half_High, Mean, T);
   end Lemma_Value_Of_Aligned_Mean;

   procedure Lemma_Value_Of_Mean (Low, High, Mean : Controls; T : Real)
   with
     Pre  =>
       Mean'Length >= 1
       and then Mean'Last < Natural'Last
       and then Low'Last < Natural'Last
       and then High'Last < Natural'Last
       and then Low'Length = Mean'Length
       and then High'Length = Mean'Length
       and then
         (for all I in Mean'Range =>
            Mean (I) = Half * Low (I - Mean'First + Low'First) + Half * High (I - Mean'First + High'First)),
     Post => Value (Mean, T) = Half * Value (Low, T) + Half * Value (High, T);
   --  Likewise when the three index ranges start at different places.

   procedure Lemma_Value_Of_Mean (Low, High, Mean : Controls; T : Real) is
      Moved_Low  : Controls (Mean'Range) := [others => 0.0];
      Moved_High : Controls (Mean'Range) := [others => 0.0];
   begin
      for I in Mean'Range loop
         Moved_Low (I) := Low (I - Mean'First + Low'First);
         Moved_High (I) := High (I - Mean'First + High'First);
         pragma
           Loop_Invariant
             (for all J in Mean'First .. I =>
                Moved_Low (J) = Low (J - Mean'First + Low'First)
                and then Moved_High (J) = High (J - Mean'First + High'First));
      end loop;
      Lemma_Value_Ignores_Offset (Moved_Low, Low, T);
      Lemma_Value_Ignores_Offset (Moved_High, High, T);
      Lemma_Value_Of_Aligned_Mean (Moved_Low, Moved_High, Mean, T);
   end Lemma_Value_Of_Mean;

   procedure Lemma_Left_Half_Prefix (P : Controls; K : Natural)
   with
     Pre  => P'Length >= 2 and then P'Last < Natural'Last and then K in P'First .. P'Last - 1,
     Post => Left_Half (P) (K) = Left_Half (P (P'First .. P'Last - 1)) (K);
   --  Dropping the last control of P drops the last control of its left half.

   procedure Lemma_Left_Half_Prefix (P : Controls; K : Natural) is
   begin
      pragma Assert (Left_Half (P) (K) = Value (P (P'First .. K), Half));
      pragma Assert (Left_Half (P (P'First .. P'Last - 1)) (K) = Value (P (P'First .. K), Half));
   end Lemma_Left_Half_Prefix;

   procedure Lemma_Left_Half_Step (P : Controls; K : Natural)
   with
     Pre  => P'Length >= 2 and then P'Last < Natural'Last and then K in P'First + 1 .. P'Last,
     Post => Left_Half (P) (K) = Half * Left_Half (P) (K - 1) + Half * Left_Half (P (P'First + 1 .. P'Last)) (K);
   --  Each control of the left half after the first is the mean of the one before it and of the matching control of
   --  the left half of P without its first control: the recurrence at one half for a leading run of P.

   procedure Lemma_Left_Half_Step (P : Controls; K : Natural) is
   begin
      Lemma_Value_Recurrence (P (P'First .. K), Half);
      pragma Assert (Left_Half (P) (K) = Value (P (P'First .. K), Half));
      pragma Assert (Left_Half (P) (K - 1) = Value (P (P'First .. K - 1), Half));
      pragma Assert (Left_Half (P (P'First + 1 .. P'Last)) (K) = Value (P (P'First + 1 .. K), Half));
   end Lemma_Left_Half_Step;

   procedure Lemma_Left_Half_Weights (A, B, T : Real)
   with Post => (1.0 - T) * A + T * (Half * A + Half * B) = (1.0 - Half * T) * A + (Half * T) * B;
   --  One step of the recurrence at T between A and the mean of A and B is one step at half of T between A and B.

   procedure Lemma_Left_Half_Weights (A, B, T : Real) is
      T_A : constant Real := T * A;
      T_B : constant Real := T * B;
   begin
      pragma Assert ((1.0 - T) * A = A - T_A);
      pragma Assert (T * (Half * A + Half * B) = Half * T_A + Half * T_B);
      pragma Assert ((1.0 - Half * T) * A = A - Half * T_A);
      pragma Assert ((Half * T) * B = Half * T_B);
   end Lemma_Left_Half_Weights;

   procedure Lemma_Left_Half (P : Controls; T : Real) is
      D : constant Controls := Left_Half (P);
   begin
      if P'Length = 1 then
         pragma Assert (D (P'First) = Value (P (P'First .. P'First), Half));
         pragma Assert (Value (P (P'First .. P'First), Half) = P (P'First));
         pragma Assert (Value (D, T) = D (D'First));
         pragma Assert (Value (P, Half * T) = P (P'First));
      else
         --  By induction on the number of controls. Write A and B for the values at half of T of P without its last
         --  and without its first control. The left half D without its last control is the left half of P without
         --  its last control, with value A at T. D without its first control is the controlwise mean of that and of
         --  E, the left half of P without its first control, with the mean of A and B as its value at T.
         declare
            P_Low  : constant Controls := P (P'First .. P'Last - 1);
            P_High : constant Controls := P (P'First + 1 .. P'Last);
            D_Low  : constant Controls := Left_Half (P_Low);
            E      : constant Controls := Left_Half (P_High);
            A      : constant Real := Value (P_Low, Half * T);
            B      : constant Real := Value (P_High, Half * T);
         begin
            Lemma_Left_Half (P_Low, T);
            Lemma_Left_Half (P_High, T);
            pragma Assert (Value (D_Low, T) = A);
            pragma Assert (Value (E, T) = B);

            for K in P'First .. P'Last - 1 loop
               Lemma_Left_Half_Prefix (P, K);
               pragma Loop_Invariant (for all J in P'First .. K => D (J) = D_Low (J));
            end loop;
            Lemma_Value_Ignores_Offset (D (D'First .. D'Last - 1), D_Low, T);
            pragma Assert (Value (D (D'First .. D'Last - 1), T) = A);

            for K in P'First + 1 .. P'Last loop
               Lemma_Left_Half_Step (P, K);
               pragma Loop_Invariant (for all J in P'First + 1 .. K => D (J) = Half * D (J - 1) + Half * E (J));
            end loop;
            Lemma_Value_Of_Mean (D (D'First .. D'Last - 1), E, D (D'First + 1 .. D'Last), T);
            pragma Assert (Value (D (D'First + 1 .. D'Last), T) = Half * A + Half * B);

            Lemma_Value_Recurrence (D, T);
            pragma Assert (Value (D, T) = (1.0 - T) * A + T * (Half * A + Half * B));
            Lemma_Value_Recurrence (P, Half * T);
            pragma Assert (Value (P, Half * T) = (1.0 - Half * T) * A + (Half * T) * B);
            Lemma_Left_Half_Weights (A, B, T);
         end;
      end if;
   end Lemma_Left_Half;

   procedure Lemma_Right_Half_Suffix (P : Controls; K : Natural)
   with
     Pre  => P'Length >= 2 and then P'Last < Natural'Last and then K in P'First + 1 .. P'Last,
     Post => Right_Half (P) (K) = Right_Half (P (P'First + 1 .. P'Last)) (K);
   --  Dropping the first control of P drops the first control of its right half.

   procedure Lemma_Right_Half_Suffix (P : Controls; K : Natural) is
   begin
      pragma Assert (Right_Half (P) (K) = Value (P (K .. P'Last), Half));
      pragma Assert (Right_Half (P (P'First + 1 .. P'Last)) (K) = Value (P (K .. P'Last), Half));
   end Lemma_Right_Half_Suffix;

   procedure Lemma_Right_Half_Step (P : Controls; K : Natural)
   with
     Pre  => P'Length >= 2 and then P'Last < Natural'Last and then K in P'First .. P'Last - 1,
     Post => Right_Half (P) (K) = Half * Right_Half (P (P'First .. P'Last - 1)) (K) + Half * Right_Half (P) (K + 1);
   --  Each control of the right half before the last is the mean of the matching control of the right half of P
   --  without its last control and of the one after it: the recurrence at one half for a trailing run of P.

   procedure Lemma_Right_Half_Step (P : Controls; K : Natural) is
   begin
      Lemma_Value_Recurrence (P (K .. P'Last), Half);
      pragma Assert (Right_Half (P) (K) = Value (P (K .. P'Last), Half));
      pragma Assert (Right_Half (P (P'First .. P'Last - 1)) (K) = Value (P (K .. P'Last - 1), Half));
      pragma Assert (Right_Half (P) (K + 1) = Value (P (K + 1 .. P'Last), Half));
   end Lemma_Right_Half_Step;

   procedure Lemma_Right_Half_Weights (A, B, T : Real)
   with Post => (1.0 - T) * (Half * A + Half * B) + T * B = (1.0 - (Half + Half * T)) * A + (Half + Half * T) * B;
   --  One step of the recurrence at T between the mean of A and B and B is one step half way from T to one between
   --  A and B.

   procedure Lemma_Right_Half_Weights (A, B, T : Real) is
      T_A : constant Real := T * A;
      T_B : constant Real := T * B;
   begin
      pragma Assert ((1.0 - T) * (Half * A + Half * B) = Half * A + Half * B - Half * T_A - Half * T_B);
      pragma Assert ((1.0 - (Half + Half * T)) * A = Half * A - Half * T_A);
      pragma Assert ((Half + Half * T) * B = Half * B + Half * T_B);
   end Lemma_Right_Half_Weights;

   procedure Lemma_Right_Half (P : Controls; T : Real) is
      D : constant Controls := Right_Half (P);
   begin
      if P'Length = 1 then
         pragma Assert (D (P'First) = Value (P (P'First .. P'Last), Half));
         pragma Assert (Value (P (P'First .. P'Last), Half) = P (P'First));
         pragma Assert (Value (D, T) = D (D'First));
         pragma Assert (Value (P, Half + Half * T) = P (P'First));
      else
         --  The mirror image of Lemma_Left_Half. Write A and B for the values half way from T to one of P without
         --  its last and without its first control. The right half D without its first control is the right half
         --  of P without its first control, with value B at T. D without its last control is the controlwise mean
         --  of E, the right half of P without its last control, and of that, with the mean of A and B as its value
         --  at T.
         declare
            X      : constant Real := Half + Half * T;
            P_Low  : constant Controls := P (P'First .. P'Last - 1);
            P_High : constant Controls := P (P'First + 1 .. P'Last);
            E      : constant Controls := Right_Half (P_Low);
            D_High : constant Controls := Right_Half (P_High);
            A      : constant Real := Value (P_Low, X);
            B      : constant Real := Value (P_High, X);
         begin
            Lemma_Right_Half (P_Low, T);
            Lemma_Right_Half (P_High, T);
            pragma Assert (Value (E, T) = A);
            pragma Assert (Value (D_High, T) = B);

            for K in P'First + 1 .. P'Last loop
               Lemma_Right_Half_Suffix (P, K);
               pragma Loop_Invariant (for all J in P'First + 1 .. K => D (J) = D_High (J));
            end loop;
            Lemma_Value_Ignores_Offset (D (D'First + 1 .. D'Last), D_High, T);
            pragma Assert (Value (D (D'First + 1 .. D'Last), T) = B);

            for K in P'First .. P'Last - 1 loop
               Lemma_Right_Half_Step (P, K);
               pragma Loop_Invariant (for all J in P'First .. K => D (J) = Half * E (J) + Half * D (J + 1));
            end loop;
            Lemma_Value_Of_Mean (E, D (D'First + 1 .. D'Last), D (D'First .. D'Last - 1), T);
            pragma Assert (Value (D (D'First .. D'Last - 1), T) = Half * A + Half * B);

            Lemma_Value_Recurrence (D, T);
            pragma Assert (Value (D, T) = (1.0 - T) * (Half * A + Half * B) + T * B);
            Lemma_Value_Recurrence (P, X);
            pragma Assert (Value (P, X) = (1.0 - X) * A + X * B);
            Lemma_Right_Half_Weights (A, B, T);
         end;
      end if;
   end Lemma_Right_Half;

end Prunt.Motion_Planner.Stereographic_Kernel.Bernstein;
