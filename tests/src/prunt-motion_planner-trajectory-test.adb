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

with Ada.Numerics.Big_Numbers.Big_Reals;
with Ada.Numerics.Long_Elementary_Functions;
with Interfaces;
with Prunt.Motion_Planner.Trajectory.Elementary;      use Prunt.Motion_Planner.Trajectory.Elementary;
with Prunt.Motion_Planner.Trajectory.Machine_Numbers; use Prunt.Motion_Planner.Trajectory.Machine_Numbers;

package body Prunt.Motion_Planner.Trajectory.Test is

   use type Interfaces.Unsigned_64;

   package Big renames Ada.Numerics.Big_Numbers.Big_Reals;
   use type Big.Big_Real;

   package Exact is new Big.Float_Conversions (Long_Float);

   function E (X : Dimensioned_Float) return Big.Big_Real;
   --  Return the exact value of a machine number.

   function E (X : Dimensioned_Float) return Big.Big_Real
   is (Exact.To_Big_Real (Long_Float (X)));

   procedure Test_Bounds (T : in out Trendy_Test.Operation'Class);
   --  Check the bounds of the logarithm, the exponential, the fifth power and the fifth root which Elementary
   --  states without proof, at the ends of their ranges and on random arguments.

   procedure Test_Machine_Numbers (T : in out Trendy_Test.Operation'Class);
   --  Check the facts about bit patterns which Machine_Numbers states without proof, on fixed and random patterns.

   procedure Test_Roots (T : in out Trendy_Test.Operation'Class);
   --  Check, in exact rational arithmetic, that the square root is one of the two machine numbers around the exact
   --  root and that the cube root is within Reals.Cube_Root_Slack of it, as Elementary states without proof.

   type Generator is record
      State : Interfaces.Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   end record;

   procedure Next (G : in out Generator; Value : out Interfaces.Unsigned_64);
   --  Advance the xorshift generator G and return its new state.

   procedure Random_Pattern (G : in out Generator; Pattern : out Bits);
   --  Return a nonnegative finite pattern, mostly anywhere in the range and sometimes at one of its ends.

   procedure Next (G : in out Generator; Value : out Interfaces.Unsigned_64) is
   begin
      G.State := G.State xor Interfaces.Shift_Left (G.State, 13);
      G.State := G.State xor Interfaces.Shift_Right (G.State, 7);
      G.State := G.State xor Interfaces.Shift_Left (G.State, 17);
      Value := G.State;
   end Next;

   procedure Random_Pattern (G : in out Generator; Pattern : out Bits) is
      Raw : Interfaces.Unsigned_64;
   begin
      Next (G, Raw);
      case Bits (Raw) mod 8 is
         when 0 =>
            Pattern := (Bits (Raw) / 8) mod 4_096;

         when 1 =>
            Pattern := Last_Pattern - (Bits (Raw) / 8) mod 4_096;

         when others =>
            Pattern := (Bits (Raw) / 8) mod (Last_Pattern + 1);
      end case;
   end Random_Pattern;

   procedure Test_Bounds (T : in out Trendy_Test.Operation'Class) is
      Cases         : constant := 10_000;
      Largest_Fifth : constant Dimensioned_Float := Fifth_Power_Argument_Max;
      G             : Generator := (State => 16#2545_F491_4F6C_DD1D#);

      procedure Check (X : Dimensioned_Float);
      --  Check the bounds at X, where each applies.

      procedure Check (X : Dimensioned_Float) is
      begin
         if X > 0.0 then
            T.Assert (abs Dimensionless_Math.Log (X) <= Log_Magnitude_Max, "Lemma_Log_Is_Bounded");
         end if;
         if X <= Exp_Argument_Max then
            T.Assert (Bounded_Exp (X) >= 0.0, "Bounded_Exp is nonnegative");
            T.Assert (Bounded_Exp (X) <= Exp_Result_Max, "Bounded_Exp is bounded");
            T.Assert (Bounded_Exp (-X) >= 0.0, "Bounded_Exp of a negative number is nonnegative");
            T.Assert (Bounded_Exp (-X) <= Exp_Result_Max, "Bounded_Exp of a negative number is bounded");
         end if;
         T.Assert
           (Cube_Root (X) = Dimensionless_Math."**" (X, 1.0 / 3.0), "Cube_Root is the power of Dimensionless_Math");
         T.Assert (Fifth_Root (X) >= 0.0, "Fifth_Root is nonnegative");
         T.Assert (Fifth_Root (X) <= Dimensioned_Float'Last, "Fifth_Root is finite");
         if X <= Largest_Fifth then
            T.Assert (Fifth_Power (X) >= 0.0, "Fifth_Power is nonnegative");
            T.Assert (Fifth_Power (X) <= 2.0**501, "Fifth_Power is bounded");
         end if;
      end Check;
   begin
      T.Register;

      Check (0.0);
      Check (Dimensioned_Float'Succ (0.0));
      Check (1.0);
      Check (Exp_Argument_Max);
      Check (Largest_Fifth);
      Check (Dimensioned_Float'Pred (Largest_Fifth));
      Check (Dimensioned_Float'Last);

      for I in 1 .. Cases loop
         declare
            Raw : Bits;
         begin
            Random_Pattern (G, Raw);
            Check (To_Real (Raw));
            --  Most patterns are far above the arguments of the exponential and the fifth power.
            Check (Dimensioned_Float (Long_Float (Raw mod 80_001) / 1_000.0));
            Check
              (Dimensioned_Float
                 (Ada.Numerics.Long_Elementary_Functions."**" (2.0, Long_Float (Raw mod 2_001) / 10.0 - 100.0)));
         end;
      end loop;
   end Test_Bounds;

   procedure Test_Machine_Numbers (T : in out Trendy_Test.Operation'Class) is
      Cases : constant := 200_000;
      G     : Generator;
   begin
      T.Register;

      T.Assert (To_Bits (0.0) = 0, "Lemma_Zero_Pattern");
      T.Assert (To_Bits (Dimensioned_Float'Last) = Last_Pattern, "Last_Pattern");
      T.Assert (To_Real (Last_Pattern) = Dimensioned_Float'Last, "To_Real (Last_Pattern)");
      T.Assert (To_Real (0) = 0.0, "To_Real (0)");
      T.Assert (To_Bits (86_400.0) = 4_680_673_776_000_565_248, "Pattern of 86400");
      T.Assert (To_Bits (0.123_45) = 4_593_559_930_647_147_132, "Pattern of 0.12345");

      for I in 1 .. Cases loop
         declare
            A, Other : Bits;
         begin
            Random_Pattern (G, A);
            if I mod 4 = 0 then
               Other := A + Bits (I mod 3);
            else
               Random_Pattern (G, Other);
            end if;
            declare
               X : constant Dimensioned_Float := To_Real (A);
               Y : constant Dimensioned_Float := To_Real ((if Other <= Last_Pattern then Other else A));
            begin
               T.Assert (X >= 0.0 and then X <= Dimensioned_Float'Last, "To_Real is nonnegative and finite");
               T.Assert ((A > 0) = (X > 0.0), "To_Real is positive exactly for a positive pattern");
               T.Assert (To_Bits (X) = A, "To_Bits (To_Real (B)) = B");
               T.Assert (To_Real (To_Bits (X)) = X, "Lemma_Round_Trip");
               T.Assert ((if X > 0.0 then To_Bits (X) in 1 .. Last_Pattern), "Lemma_Positive_Pattern");
               T.Assert (To_Bits (To_Time (A)) = A and then To_Time (A) / s = X, "To_Time agrees with To_Real");
               T.Assert ((X < Y) = (To_Bits (X) < To_Bits (Y)), "Lemma_Order, less");
               T.Assert ((X = Y) = (To_Bits (X) = To_Bits (Y)), "Lemma_Order, equal");
               if A < Last_Pattern then
                  T.Assert
                    (To_Real (A + 1) = Dimensioned_Float'Succ (X), "The next pattern is the next machine number");
               end if;
            end;
         end;
      end loop;
   end Test_Machine_Numbers;

   procedure Test_Roots (T : in out Trendy_Test.Operation'Class) is
      Cases : constant := 10_000;
      One   : constant Big.Big_Real := Big.To_Real (1);
      Slack : constant Big.Big_Real := One / Big.To_Real (2)**40;
      --  Reals.Cube_Root_Slack.
      G     : Generator := (State => 16#D6E8_FEB8_6659_FD93#);
   begin
      T.Register;

      for I in 1 .. Cases loop
         declare
            Raw : Bits;
            X   : Dimensioned_Float;
            Y   : Dimensioned_Float;
            C   : Dimensioned_Float;
         begin
            Random_Pattern (G, Raw);
            X := To_Real (Raw);
            case I mod 5 is
               when 0 =>
                  --  Exactly or almost a square of a machine number.
                  declare
                     Root : constant Long_Float :=
                       Ada.Numerics.Long_Elementary_Functions."**" (10.0, Long_Float (Raw mod 400) / 10.0 - 20.0);
                  begin
                     X := Dimensioned_Float (Root * Root);
                     if I mod 3 = 1 then
                        X := Dimensioned_Float'Succ (X);
                     elsif I mod 3 = 2 then
                        X := Dimensioned_Float'Pred (X);
                     end if;
                  end;

               when 1 =>
                  X :=
                    Dimensioned_Float
                      (Ada.Numerics.Long_Elementary_Functions."**"
                         (10.0, Long_Float (Raw mod 6_000) / 100.0 - 30.0));

               when others =>
                  null;
            end case;

            Y := Dimensionless_Math.Sqrt (X);
            T.Assert (Y >= 0.0, "Sqrt is nonnegative");
            if Y < Dimensioned_Float'Last then
               T.Assert
                 (E (Dimensioned_Float'Succ (Y)) * E (Dimensioned_Float'Succ (Y)) > E (X),
                  "Lemma_Square_Root_Is_Faithful, from below");
            end if;
            if Y > 0.0 then
               T.Assert
                 (E (Dimensioned_Float'Pred (Y)) * E (Dimensioned_Float'Pred (Y)) < E (X),
                  "Lemma_Square_Root_Is_Faithful, from above");
            else
               T.Assert (X = 0.0, "Sqrt is zero only at zero");
            end if;

            C := Cube_Root (X);
            T.Assert (C >= 0.0, "Cube_Root is nonnegative");
            declare
               Above : constant Big.Big_Real := E (C) / (One - Slack);
               Below : constant Big.Big_Real := E (C) / (One + Slack);
            begin
               T.Assert (Above * Above * Above >= E (X), "Lemma_Cube_Root_Is_Close, from below");
               T.Assert (Below * Below * Below <= E (X), "Lemma_Cube_Root_Is_Close, from above");
            end;
         end;
      end loop;
   end Test_Roots;

   function All_Tests return Trendy_Test.Test_Group
   is ([Test_Bounds'Access, Test_Machine_Numbers'Access, Test_Roots'Access]);

end Prunt.Motion_Planner.Trajectory.Test;
