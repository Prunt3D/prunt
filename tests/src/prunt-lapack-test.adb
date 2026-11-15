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

with Ada.Unchecked_Conversion;
with Interfaces;
with Prunt.LAPACK.Elementary; use Prunt.LAPACK.Elementary;

package body Prunt.LAPACK.Test is

   use type Interfaces.Unsigned_64;

   function To_Real is new Ada.Unchecked_Conversion (Interfaces.Unsigned_64, Real);

   Exponent_Mask : constant Interfaces.Unsigned_64 := 16#7FF0_0000_0000_0000#;
   Fraction_Mask : constant Interfaces.Unsigned_64 := 16#000F_FFFF_FFFF_FFFF#;

   Negative_Zero : constant Real := To_Real (16#8000_0000_0000_0000#);

   procedure Test_Sqrt (T : in out Trendy_Test.Operation'Class);
   --  Check the contract of Sqrt and its two lemmas, which Elementary states without proof, at the ends of their
   --  ranges and on random arguments.

   procedure Test_Sign (T : in out Trendy_Test.Operation'Class);
   --  Check the contract of Sign, including the sign bit of a zero Source.

   type Generator is record
      State : Interfaces.Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   end record;

   procedure Next (G : in out Generator; Value : out Interfaces.Unsigned_64);
   --  Advance the xorshift generator G and return its new state.

   procedure Random_Nonnegative (G : in out Generator; X : out Real);
   --  Return a nonnegative finite number, mostly anywhere in the range, denormalized numbers included, and sometimes
   --  within a few machine numbers of zero or of the largest number.

   procedure Next (G : in out Generator; Value : out Interfaces.Unsigned_64) is
   begin
      G.State := G.State xor Interfaces.Shift_Left (G.State, 13);
      G.State := G.State xor Interfaces.Shift_Right (G.State, 7);
      G.State := G.State xor Interfaces.Shift_Left (G.State, 17);
      Value := G.State;
   end Next;

   procedure Random_Nonnegative (G : in out Generator; X : out Real) is
      Raw     : Interfaces.Unsigned_64;
      Largest : constant Interfaces.Unsigned_64 := Exponent_Mask - 1;
   begin
      Next (G, Raw);
      case Raw mod 8 is
         when 0 =>
            X := To_Real ((Raw / 8) mod 4_096);

         when 1 =>
            X := To_Real (Largest - (Raw / 8) mod 4_096);

         when others =>
            X := To_Real ((Raw / 8) mod (Largest + 1));
      end case;
   end Random_Nonnegative;

   procedure Test_Sqrt (T : in out Trendy_Test.Operation'Class) is
      Cases : constant := 200_000;
      G     : Generator;

      procedure Check (X, Y : Real);
      --  Check the contract of Sqrt at X and Y, and its monotonicity between them, where 0 <= X <= Y.

      procedure Check (X, Y : Real) is
      begin
         T.Assert (Sqrt (X) >= 0.0, "Sqrt is nonnegative");
         T.Assert ((Sqrt (X) > 0.0) = (X > 0.0), "Sqrt is zero exactly for zero");
         T.Assert (Sqrt (X) <= Sqrt (Y), "Lemma_Sqrt_Is_Monotonic");
      end Check;
   begin
      T.Register;

      Check (0.0, 0.0);
      Check (Negative_Zero, 0.0);
      Check (0.0, Real'Succ (0.0));
      Check (Real'Pred (Real'Last), Real'Last);
      T.Assert (Sqrt (Exact_Square_Minimum * Exact_Square_Minimum) = Exact_Square_Minimum, "Lemma_Sqrt_Of_Square");
      T.Assert (Sqrt (Exact_Square_Maximum * Exact_Square_Maximum) = Exact_Square_Maximum, "Lemma_Sqrt_Of_Square");

      for I in 1 .. Cases loop
         declare
            X, Y : Real;
            Raw  : Interfaces.Unsigned_64;
         begin
            Random_Nonnegative (G, X);
            if I mod 2 = 0 then
               Y := (if X < Real'Last then Real'Succ (X) else X);
            else
               Random_Nonnegative (G, Y);
            end if;
            Check (Real'Min (X, Y), Real'Max (X, Y));

            --  A number with a random fraction and an exponent from -511 to 510, so within Exact_Square_Minimum and
            --  Exact_Square_Maximum.
            Next (G, Raw);
            X :=
              To_Real
                (Interfaces.Shift_Left (Interfaces.Unsigned_64 (1_023 - 511) + (Raw / 2**52) mod 1_022, 52)
                 or (Raw and Fraction_Mask));
            T.Assert (X >= Exact_Square_Minimum and then X <= Exact_Square_Maximum, "The argument is in range");
            T.Assert (Sqrt (X * X) = X, "Lemma_Sqrt_Of_Square");
         end;
      end loop;
   end Test_Sqrt;

   procedure Test_Sign (T : in out Trendy_Test.Operation'Class) is
      Cases : constant := 10_000;
      G     : Generator := (State => 16#2545_F491_4F6C_DD1D#);

      procedure Check (Magnitude, Source : Real);
      --  Check the contract of Sign for Magnitude and Source.

      procedure Check (Magnitude, Source : Real) is
         Result : constant Real := Sign (Magnitude, Source);
      begin
         T.Assert (abs Result = abs Magnitude, "Sign keeps the magnitude");
         T.Assert ((if Source > 0.0 then Result >= 0.0), "Sign of a positive Source");
         T.Assert ((if Source < 0.0 then Result <= 0.0), "Sign of a negative Source");
      end Check;
   begin
      T.Register;

      T.Assert (Sign (1.0, Negative_Zero) = -1.0, "Sign of a negative zero Source is negative");
      T.Assert (Sign (-1.0, 0.0) = 1.0, "Sign of a zero Source is positive");

      for I in 1 .. Cases loop
         declare
            Magnitude, Source : Real;
         begin
            Random_Nonnegative (G, Magnitude);
            Random_Nonnegative (G, Source);
            Check (Magnitude, Source);
            Check (-Magnitude, Source);
            Check (Magnitude, -Source);
            Check (-Magnitude, -Source);
         end;
      end loop;
   end Test_Sign;

   function All_Tests return Trendy_Test.Test_Group
   is ([Test_Sqrt'Access, Test_Sign'Access]);

end Prunt.LAPACK.Test;
